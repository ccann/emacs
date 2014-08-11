(add-hook 'qichat-mode-hook (lambda ()
                              (linum-mode)
                              (auto-fill-mode -1)
                              (subword-mode 1)))
