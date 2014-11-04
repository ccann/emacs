;;; keybindings.el --- contains keybindings for emacs

;;; Commentary:
;;;

;;; Code:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs) 
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-c C-r C-b") 'org-bibtex-read-buffer)
(global-set-key (kbd "C-c C-w") 'org-bibtex-write)
(global-set-key (kbd "C-c k") 'compile)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c d") 'jedi:show-doc)
(global-set-key (kbd "M-x") 'smex)
(global-set-key [(f12)] 'ibuffer)
(global-set-key [(f10)] 'magit-status)
(global-set-key [(f9)] 'cycle-my-theme)
(global-set-key [(f6)] (lambda () (interactive) (enab-theme 'flatui)))
(global-set-key [(f5)] (lambda () (interactive) (enab-theme 'badger)))


;; Home and End Keys:
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; backspace no longer does reverse-search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;;; keybindings.el ends here
