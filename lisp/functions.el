;;; functions.el --- miscellaneous function definitions

;;; Commentary: 
;;; author: ccann

;;; Code: 

;; pulled these from someone... don't remember who!
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(setq curr-theme nil)

(defun enab-theme (theme)
  "Disable theme then load it, clearing out state of previous theme"
  (if curr-theme (disable-theme curr-theme))
  (setq curr-theme theme) 
  (load-theme theme t))

(defun cycle-my-theme ()
  "Cycle through a list of themes, my-themes"
  (interactive)
  (when curr-theme
    (disable-theme curr-theme)
    (setq my-themes (append my-themes (list curr-theme))))
  (setq curr-theme (pop my-themes))
  (load-theme curr-theme t)
  (sml/apply-theme 'respectful))


(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))

;;; functions.el ends here
