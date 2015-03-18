(add-hook 'clojure-mode-hook (lambda ()
                               (paredit-mode 1)
                               (rainbow-delimiters-mode 1)))

(add-hook 'cider-mode-hook (lambda ()
                             #'eldoc-mode ;; Enable eldoc in Clojure buffers
                             #'company-mode
                             ))

(add-hook 'cider-repl-mode-hook #'company-mode)


;; Log communication with the nREPL server
(setq nrepl-log-messages t)

;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
;; (setq cider-repl-pop-to-buffer-on-connect t)

(setq cider-prompt-save-file-on-load nil)
