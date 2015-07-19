(add-hook 'clojure-mode-hook (lambda ()
                               (smartparens-mode)
                               (sp-use-smartparens-bindings)
                               (require 'smartparens-config)
                               (rainbow-delimiters-mode 1)
                               (paredit-mode 0)
                               (linum-mode 1)
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1) ; for adding require/use/import
                               (cljr-add-keybindings-with-prefix "C-c C-m")
                               (setq sp-override-key-bindings '(("C-<right>" . nil)
                                                                ("C-<left>" . nil)
                                                                ("C-(" . sp-forward-slurp-sexp)
                                                                ("M-<backspace>" . nil)
                                                                ("C-)" . sp-forward-barf-sexp)))
                               ))

(add-hook 'cider-mode-hook (lambda ()
                             (eldoc-mode 1)
                             (paredit-mode 0)
                             (company-mode 1)))

(add-hook 'cider-repl-mode-hook (lambda ()
                                  (company-mode 1)))

;; Log communication with the nREPL server
(setq nrepl-log-messages t)

;; Make C-c C-z switch to the CIDER REPL buffer in the current window:
(setq cider-repl-display-in-current-window t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established

;; (setq cider-repl-pop-to-buffer-on-connect t)

(setq cider-prompt-save-file-on-load nil)

(require 'clj-refactor)
