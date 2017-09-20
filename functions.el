;;; functions.el --- miscellaneous function definitions

;;; Commentary:
;;; author: ccann

;;; Code:

;;pulled these from someone... don't remember who!
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(defun find-user-init-file ()
  "Edit the `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun toggle-case-camel-kebab ()
  "Toggle between camelcase and kebab notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-dashes-p (progn (goto-char start)
                                                 (re-search-forward "-" end t))))
      (if currently-using-dashes-p
          (progn
            (upcase-initials-region start end)
            (replace-string "-" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "-\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun ccann/toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))

(defun find-notes-file ()
  (interactive)
  (progn
    (find-file (concat org-directory "/notes.org"))
    (ccann/toggle-org-html-export-on-save)))


(defun ccann/cycle-theme ()
  "Cycle through a list of themes, my-themes."
  (interactive)
  (when curr-theme
    (disable-theme curr-theme)
    (setq my-themes (append my-themes (list curr-theme))))
  (setq curr-theme (pop my-themes))
  (load-theme curr-theme t)
  ;; (sml/apply-theme 'respectful)
  ;; (underline-straight)
  ;; (setq highlight-symbol-foreground-color "#000000")
  ;; (when (eq curr-theme 'challenger-deep)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (when (eq curr-theme 'darktooth)
    (custom-set-faces '(vertical-border ((t (:foreground "#504945"))))))
  (when (eq curr-theme 'doom-one) (solaire-mode-swap-bg)))

; show-paren-match


(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer."
  (interactive)
  (ivy-yank-word
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))


(defun underline-straight ()
  (progn
    (setq curr-underline-color (cdr (assoc :color (loop for (head . tail) on (face-attribute 'flycheck-warning :underline) by 'cddr collect (cons head (car tail))))))
    (set-face-attribute 'flycheck-warning nil :underline `(:color ,curr-underline-color :style line))
    (setq curr-underline-color (cdr (assoc :color (loop for (head . tail) on (face-attribute 'flycheck-error :underline) by 'cddr collect (cons head (car tail))))))
    (set-face-attribute 'flycheck-error nil :underline `(:color ,curr-underline-color :style line))
    (setq curr-underline-color (cdr (assoc :color (loop for (head . tail) on (face-attribute 'flycheck-info :underline) by 'cddr collect (cons head (car tail))))))
    (set-face-attribute 'flycheck-info nil :underline `(:color ,curr-underline-color :style line))
    ;; change this to nlinum
    ;; (set-face-attribute 'linum nil :underline nil)
    ))


(defun ccann/get-envs (filename)
  (let* ((cmd (concat ". " filename "; env"))
         (env-str (shell-command-to-string cmd))
         (env-lines (split-string env-str "\n"))
         (envs (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-lines)))
    (delete "" envs)))

;;; functions.el ends here
