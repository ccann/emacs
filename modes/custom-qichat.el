(add-to-list 'auto-mode-alist '("\\.madchat\\'" . qichat-mode))

(add-hook 'qichat-mode-hook (lambda ()
                              (linum-mode)
                              (auto-fill-mode -1)
                              (subword-mode 1)
                              (fci-mode -1)))