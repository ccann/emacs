(use-package lsp-mode
  :hook ((java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-l"))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
  :after (lsp-mode))

(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(provide 'init-java)
