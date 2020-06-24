;;; init-basic.el --- Basic configuration

;;; Commentary:
;;; author: ccann

;;; Code:

;; -- Constants --
(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; https://gist.github.com/railwaycat/3498096
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)] (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c N") 'find-notes-file)
(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; (defun ccann/get-envs (filename)
;;   (let* ((cmd (concat ". " filename "; env"))
;;          (env-str (shell-command-to-string cmd))
;;          (env-lines (split-string env-str "\n"))
;;          (envs (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-lines)))
;;     (delete "" envs)))

;; (use-package exec-path-from-shell
;;   :init
;;   (setq exec-path-from-shell-check-startup-files nil)
;;   (exec-path-from-shell-copy-envs (ccann/get-envs "~/.profile"))
;; ;  (exec-path-from-shell-copy-env "PYTHONPATH")
;;   (exec-path-from-shell-initialize))

;; (use-package exec-path-from-shell
;;   :defer 1
;;   :init (setq exec-path-from-shell-check-startup-files nil)
;;   :config
;;   (when sys/mac-cocoa-p
;;     (exec-path-from-shell-initialize)
;;     (exec-path-from-shell-copy-envs (ccann/get-envs "~/.profile"))
;;     (exec-path-from-shell-copy-env "PYTHONPATH")))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init (setq display-time-24hr-format t
              display-time-day-and-date t))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(setq column-number-mode 1
      line-move-visual nil
      track-eol t                    ; Keep cursor at end of lines. Require line-move-visual is nil.
      set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again

(setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq-default display-line-numbers-type t
              display-line-numbers-width 3)


(fset 'yes-or-no-p 'y-or-n-p)
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              ;; Permanently indent with spaces
              indent-tabs-mode nil)

;; empty alarm function. voila.
(setq ring-bell-function `(lambda () ))

(setq visible-bell nil
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      ;; make-backup-files nil             ; Forbid to make backup files
      ;; auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Fullscreen
;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen."
  (and sys/mac-cocoa-p
       (setq ns-use-native-fullscreen nil)))

(when (display-graphic-p)
  (add-hook 'window-setup-hook #'fix-fullscreen-cocoa))

(provide 'init-basic)

;;; init-basic.el ends here
