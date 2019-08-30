(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel-projectile zoom popup-kill-ring golden-ratio clj-refactor cider auctex-latexmk auctex zenburn-theme yaml-mode xref-js2 which-key web-mode visual-regexp-steroids use-package terraform-mode spacemacs-theme robe rainbow-delimiters python-mode projectile-ripgrep powerline pov-mode paren-face panda-theme omnisharp olivetti ob-ipython nyan-mode neotree metalheart-theme material-theme markdown-mode magit lua-mode list-utils lispy key-chord kaolin-themes js2-refactor jedi ivy-hydra ido-vertical-mode htmlize highlight-symbol highlight-numbers handlebars-mode gruvbox-theme gotham-theme google-this god-mode git-timemachine flymake-shellcheck flycheck-joker flycheck-clj-kondo flx flatui-theme fill-column-indicator eyebrowse exec-path-from-shell elpy dockerfile-mode diminish darktooth-theme company-tern company-anaconda clojure-mode cider-eval-sexp-fu challenger-deep-theme browse-kill-ring badger-theme apropospriate-theme ample-theme all-the-icons adoc-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-clojure-cli-global-options . "-A:dev:test:frontend:dev/frontend")
     (cider-cljs-repl-types
      (edge "(do (require 'dev-extras) ((resolve 'dev-extras/cljs-repl) \"app\"))"))
     (cider-default-cljs-repl . edge)
     (cider-clojure-cli-global-options . "-A:dev:test:frontend:dev/build")
     (cider-repl-init-code "(dev)")
     (cider-ns-refresh-after-fn . "dev/resume")
     (cider-ns-refresh-before-fn . "dev/suspend")
     (cider-ns-refresh-after-fn . "mount.core/start")
     (cider-ns-refresh-before-fn . "mount.core/stop")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
