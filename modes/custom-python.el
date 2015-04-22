(when (eq system-type 'darwin)
  (setq elpy-rpc-python-command "/usr/local/bin/python"))

;; set the PYTHONPATH to be what the shell has
(let ((path-from-shell (shell-command-to-string "echo $PYTHONPATH")))
  (setenv "PYTHONPATH" (mapconcat 'identity (split-string path-from-shell) ":")
    ))

;; set the LD_LIBRARY_PATH to be what the shell has (necessary for naoqi)
(let ((path-from-shell (shell-command-to-string "echo $LD_LIBRARY_PATH")))
  (setenv "LD_LIBRARY_PATH" (mapconcat 'identity (split-string path-from-shell) ":")
    ))

(add-to-list 'auto-mode-alist '("\\.xar\\'" . python-mode))
(add-hook 'after-init-hook 'global-company-mode)
;;(eval-after-load 'company '(add-to-list 'company-backends 'company-anaconda))

;; don't guess the indent offset
(setq-default python-indent-guess-indent-offset nil)
;; just use 4 spaces
(setq-default python-indent-offset 4)

(add-hook 'python-mode-hook (lambda ()
                              (elpy-enable)
                              (elpy-mode 1)
                              (setq python-fill-docstring-style 'pep-257-nn)
                              (setq python-check-command "flake8")
;;                              (anaconda-mode 1)
                              ))

;; cherry-pick some ELPY modules
(setq elpy-modules '(elpy-module-company
                     elpy-module-eldoc
                     elpy-module-sane-defaults))

(add-hook 'elpy-mode-hook (lambda ()
                            (subword-mode 1)              ;; camelCase words
                            (linum-mode 1)                ;; line numbering
                            ;; (company-mode 1)              ;; auto-completion
                            (rainbow-delimiters-mode 1)   ;; colored matching parens
                            (elpy-use-ipython)            ;; use ipython as interpreter
                            (fci-mode -1)                 ;; fill-column-indicator
                            (flycheck-mode 1)
                            (auto-fill-mode -1)))

(setq jedi:complete-on-dot t)

;;; custom-python.el ends here
