(when (eq system-type 'darwin)
  (setq elpy-rpc-python-command "/usr/local/bin/python"))

;; set the PYTHONPATH to be what the shell has
(let ((path-from-shell (shell-command-to-string "echo $PYTHONPATH")))
  (setenv "PYTHONPATH" (mapconcat 'identity (split-string path-from-shell) ":")
    ))

(setq python-check-command "flake8")

;; don't guess the indent offset
(setq-default python-indent-guess-indent-offset nil)
;; just use 4 spaces
(setq-default python-indent-offset 4)

(add-hook 'python-mode-hook (lambda ()
                              (elpy-enable)
                              (elpy-mode 1)))

(add-hook 'elpy-mode-hook (lambda ()
                            (subword-mode 1) ;; camelCase words
                            (linum-mode 1)   ;; line numbering
                            (company-mode 1) ;; auto-completion
                            (rainbow-delimiters-mode 1) ;; colored matching parens
                            (elpy-use-ipython)  ;; use ipython as interpreter
                            (fci-mode -1) ;; fill-column-indicator
                            (highlight-indentation-mode -1) ;; so ugly
                            (auto-fill-mode -1))) 

(setq jedi:complete-on-dot t)
