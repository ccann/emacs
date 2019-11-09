;;; eftest-runner.el --- A package for running Eftest tests via CIDER. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "26") (s "1.4.0") (cider "0.21.0") (parseedn "0.1.0"))
;; URL: https://github.com/magnars/eftest-runner.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor-mode for running Eftest tests with CIDER

;;; Code:

(require 'cider)
(require 'parseedn)
(require 's)

(defgroup eftest-runner nil
  "Run Eftest tests via CIDER."
  :group 'tools)

(defcustom eftest-runner-repl-invocation-template
  "(do (require 'eftest.runner) %s)"
  "The invocation sent to the REPL to run eftest tests, with the actual run replaced by %s."
  :group 'eftest-runner
  :type 'string)

(defcustom eftest-runner-extra-configuration
  "{:eftest/fail-fast? true :multithread? :vars}"
  "Extra configuration options passed to eftest, a string containing an edn map."
  :group 'eftest-runner
  :type 'string)

(defcustom eftest-runner-long-running-seconds
  3
  "After a test run has taken this many seconds, pop up the output window to see what is going on."
  :group 'eftest-runner
  :type 'integer
  :package-version '(eftest-runner . "0.3.0"))

(defcustom eftest-runner-ongoing-tests-win-min-height
  12
  "The minimum height in lines of the output window when tests are taking long to run.
This is to show the ongoing progress from eftest."
  :group 'eftest-runner
  :type 'integer
  :package-version '(eftest-runner . "0.2.0"))

(defcustom eftest-runner-failure-win-min-height
  4
  "The minimum height in lines of the output window when there are failing tests."
  :group 'eftest-runner
  :type 'integer
  :package-version '(eftest-runner . "0.2.0"))

(defcustom eftest-runner-output-win-max-height
  16
  "The maximum height in lines of the output window."
  :group 'eftest-runner
  :type 'integer
  :package-version '(eftest-runner . "0.2.0"))

(defface eftest-runner-success-face
  '((t (:foreground "green")))
  "Face used to highlight success messages."
  :group 'eftest-runner)

(defface eftest-runner-error-face
  '((t (:foreground "red")))
  "Face used to highlight error messages."
  :group 'eftest-runner)

(defface eftest-runner-warning-face
  '((t (:foreground "yellow")))
  "Face used to highlight warning messages."
  :group 'eftest-runner)

(defun eftest-runner--eval-clojure-code (code callback)
  "Send CODE to be evaled and run to CIDER, calling CALLBACK with updates."
  (cider-nrepl-request:eval
   code
   callback
   nil nil nil nil
   (cider-current-repl 'clj 'ensure)))

(defvar eftest-runner--out-buffer "*eftest-output*")
(defvar eftest-runner--err-buffer "*eftest-error*")

(defun eftest-runner--clear-buffer (buffer)
  "Ensure that BUFFER exists and is empty."
  (get-buffer-create buffer)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))))

(defun eftest-runner--colorize ()
  "Turn ANSI codes in the current buffer into Emacs colors."
  (save-excursion
    (goto-char (point-min))
    (insert "[m")
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun eftest-runner--insert (buffer s)
  "Insert S into BUFFER, then turn ANSI codes into color."
  (with-current-buffer buffer
    (insert s)
    (eftest-runner--colorize)))

(defmacro eftest-runner--with-window (buffer original-buffer &rest body)
  "Open a dedicated window showing BUFFER, perform BODY, then switch back to ORIGINAL-BUFFER."
  (declare (debug (form body))
           (indent 2))
  `(let ((window (get-buffer-window ,buffer)))
     (if window
         (select-window window)
       (let ((window (split-window-vertically -4)))
         (select-window window)
         (switch-to-buffer ,buffer)
         (set-window-dedicated-p window t)))
     ,@body
     (switch-to-buffer-other-window ,original-buffer)))

(defun eftest-runner--fit-window-snuggly (min-height max-height)
  "Resize current window to fit its contents, within MIN-HEIGHT and MAX-HEIGHT."
  (window-resize nil (- (max min-height
                             (min max-height
                                  (- (line-number-at-pos (point-max))
                                     (line-number-at-pos (point)))))
                        (window-height))))

(defun eftest-runner--recenter-top ()
  "Change the scroll position so that the cursor is at the top of the window."
  (recenter (min (max 0 scroll-margin)
                 (truncate (/ (window-body-height) 4.0)))))

(defun eftest-runner--num-warnings ()
  "Count the number of warnings in the error buffer."
  (s-count-matches "WARNING:"
                   (with-current-buffer eftest-runner--err-buffer
                     (buffer-substring-no-properties (point-min) (point-max)))))

(defun eftest-runner--show-report (value testable-sym)
  "Show a message detailing the test run restult in VALUE, prefixed by TESTABLE-SYM"
  (when-let* ((result (parseedn-read-str (s-chop-prefix "#:eftest.result" value))))
    (let* ((tests (gethash :count result))
           (pass (gethash :pass result))
           (fail (gethash :fail result))
           (err (gethash :error result))
           (warnings (eftest-runner--num-warnings))
           (happy? (and (= 0 fail) (= 0 err)))
           (report (format "%s%s"
                           (if testable-sym
                               (concat "[" testable-sym "] ")
                             "")
                           (propertize (format "%s tests, %s assertions%s, %s failures."
                                               tests
                                               (+ pass fail err)
                                               (if (< 0 err)
                                                   (format ", %s errors" err)
                                                 "")
                                               fail)
                                       'face (if happy?
                                                 'eftest-runner-success-face
                                               'eftest-runner-error-face)))))
      (when (< 0 warnings)
        (let ((warnings-str (format "(%s warnings)" warnings)))
          (setq report (concat report (s-repeat (max 3 (- (frame-width) (length report) (length warnings-str))) " ")
                               (propertize warnings-str 'face 'eftest-runner-warning-face)))))
      (message "%s" report))))

(defvar eftest-runner--fail-re "\\(FAIL\\|ERROR\\)")

(defun eftest-runner--show-details-window (original-buffer min-height)
  "Show details from the test run with a MIN-HEIGHT, but switch back to ORIGINAL-BUFFER afterwards."
  (eftest-runner--with-window eftest-runner--out-buffer original-buffer
    (visual-line-mode 1)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (re-search-forward eftest-runner--fail-re nil t))
    (end-of-line)
    (eftest-runner--fit-window-snuggly min-height eftest-runner-output-win-max-height)
    (eftest-runner--recenter-top)))

(defun eftest-runner--testable-sym (ns test-name cljs?)
  (concat "'"
          (if cljs? "cljs:" "")
          ns
          (when test-name (concat "/" test-name))))

(defun eftest-runner--hide-window (buffer-name)
  (when-let (buffer (get-buffer buffer-name))
    (when-let (window (get-buffer-window buffer))
      (delete-window window))))

(defun eftest-runner--run-tests (testable-sym &optional run-all? background? original-buffer)
  "Run eftest tests.

If RUN-ALL? is t, all tests are run, otherwise attempt a run with the provided
TESTABLEY-SYM. In practice TESTABLEY-SYM can be a test id, an ns or an ns/test-fn.

If BACKGROUND? is t, we don't message when the tests start running.

Given an ORIGINAL-BUFFER, use that instead of (current-buffer) when switching back."
  (interactive)
  (eftest-runner--clear-buffer eftest-runner--out-buffer)
  (eftest-runner--clear-buffer eftest-runner--err-buffer)
  (eftest-runner--eval-clojure-code
   (format eftest-runner-repl-invocation-template
           (if run-all?
               (format "(eftest.runner/run-tests (eftest.runner/find-tests \"test\") %s)" eftest-runner-extra-configuration)
             (format
              "(eftest.runner/run-tests (eftest.runner/find-tests %s) %s)"
              testable-sym
              eftest-runner-extra-configuration)))
   (let ((original-buffer (or original-buffer (current-buffer)))
         (done? nil)
         (any-errors? nil)
         (shown-details? nil)
         (the-value nil)
         (start-time (float-time)))
     (unless background?
       (if run-all?
           (message "Running all tests ...")
         (message "[%s] Running tests ..." testable-sym)))
     (lambda (response)
       (nrepl-dbind-response response (value out err status)
         (when out
           (eftest-runner--insert eftest-runner--out-buffer out)
           (when (let ((case-fold-search nil))
                   (string-match-p eftest-runner--fail-re out))
             (setq any-errors? t))
           (when (and (< eftest-runner-long-running-seconds
                         (- (float-time) start-time))
                      (not shown-details?))
             (setq shown-details? t)
             (eftest-runner--show-details-window original-buffer eftest-runner-ongoing-tests-win-min-height)))
         (when err
           (eftest-runner--insert eftest-runner--err-buffer err))
         (when value
           (setq the-value value))
         (when (and status (member "done" status))
           (setq done? t))
         (when done?
           (if the-value
               (eftest-runner--show-report the-value (unless run-all? testable-sym))
             (unless (get-buffer-window eftest-runner--err-buffer 'visible)
               (message "Eftest run failed. See error window for details.")
               (switch-to-buffer-other-window eftest-runner--err-buffer))))
         (when done?
           (if any-errors?
               (eftest-runner--show-details-window original-buffer eftest-runner-failure-win-min-height)
             (eftest-runner--hide-window eftest-runner--out-buffer))))))))

;;;###autoload
(defun eftest-runner-hide-windows ()
  "Hide all windows that eftest has opened."
  (interactive)
  (eftest-runner--hide-window eftest-runner--out-buffer)
  (eftest-runner--hide-window eftest-runner--err-buffer))

;;;###autoload
(defun eftest-runner-run-tests (&optional test-id?)
  "Run tests in the current namespace.
If prefix argument TEST-ID? is present ask user for a test-id to run."
  (interactive "P")
  (eftest-runner-hide-windows)
  (let ((test-id (when test-id? (read-from-minibuffer "test id: "))))
    (eftest-runner--run-tests
     (if test-id
         test-id
       (eftest-runner--testable-sym (cider-current-ns) nil (eq major-mode 'clojurescript-mode))))))

;;;###autoload
(defun eftest-runner-run-test-at-point ()
  "Run the test at point in the current namespace."
  (interactive)
  (eftest-runner-hide-windows)
  (eftest-runner--run-tests
   (eftest-runner--testable-sym (cider-current-ns) (cadr (clojure-find-def)) (eq major-mode 'clojurescript-mode))))

;;;###autoload
(defun eftest-runner-run-all-tests ()
  "Run all tests."
  (interactive)
  (eftest-runner-hide-windows)
  (eftest-runner--run-tests nil t))

;;;###autoload
(defun eftest-runner-show-warnings (&optional switch-to-buffer?)
  "Display warnings from the last eftest test run.
Prefix argument SWITCH-TO-BUFFER? opens a separate window."
  (interactive "P")
  (if switch-to-buffer?
      (switch-to-buffer-other-window eftest-runner--err-buffer)
    (message "%s"
             (s-trim
              (with-current-buffer eftest-runner--err-buffer
                (buffer-substring (point-min) (point-max)))))))

(provide 'eftest-runner)
;;; eftest-runner.el ends here
