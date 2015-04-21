(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode 1)
                                  (flycheck-mode 1)
                                  (linum-mode 1)
                                  (company-mode 1)
                                  (rainbow-delimiters-mode 1)))

