;;; core-extensions.el --- Extensions to Emacs

;;; Commentary: 
;;; author: ccann

;;; Code: 

;;;###autoload
(progn

  (ido-mode t)
  (setq ido-enable-flex-matching t)

  ;; smart meta-x (use IDO in minibuffer)
  (smex-initialize)
  ;; change to 'y' or 'n'
  (fset 'yes-or-no-p 'y-or-n-p) 

  (setq ibuffer-shrink-to-minimum-size t)
  (setq ibuffer-always-show-last-buffer nil)
  (setq ibuffer-sorting-mode 'recency)
  (setq ibuffer-use-header-line t)
    
  (setq ls-lisp-use-insert-directory-program t)
  (setq dired-use-ls-dired nil) 
  (require 'dired-details)
  (dired-details-install)

  ;; SMART-MODE-LINE
  (sml/setup)
  (setq sml/mule-info nil)
  (setq sml/numbers-separator "")
  (setq sml/show-remote nil)
  (setq sml/modified-char " x ")

  ;; use prepended directory instead of silly filename<2>
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; save place in the buffer for next visit
  (require 'saveplace)
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places"))

  ;; magit
  (when (eq system-type 'darwin)
    (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))


  
)
(provide 'core-extensions)

;;; core-extensions.el ends here
