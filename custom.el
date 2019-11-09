(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ecf0f1" "#e74c3c" "#2ecc71" "#f1c40f" "#2492db" "#9b59b6" "#1abc9c" "#2c3e50"])
 '(custom-safe-themes
   (quote
    ("11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" "cdb4ffdecc682978da78700a461cdc77456c3a6df1c1803ae2dd55c59fa703e3" default)))
 '(fci-rule-color "#f1c40f")
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#D08770"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#4f5b66"))
 '(objed-cursor-color "#BF616A")
 '(package-selected-packages
   (quote
    (direnv counsel-dash dash-at-point grip-mode rainbow-mode ivy-prescient prescient ivy-posframe treemacs-magit treemacs-projectile treemacs doom-themes doom-modeline counsel-projectile zoom popup-kill-ring golden-ratio auctex-latexmk auctex zenburn-theme yaml-mode xref-js2 which-key web-mode visual-regexp-steroids use-package terraform-mode spacemacs-theme robe rainbow-delimiters python-mode projectile-ripgrep powerline pov-mode paren-face panda-theme omnisharp olivetti ob-ipython nyan-mode neotree metalheart-theme material-theme markdown-mode magit lua-mode list-utils lispy key-chord kaolin-themes js2-refactor jedi ivy-hydra ido-vertical-mode htmlize highlight-symbol highlight-numbers handlebars-mode gruvbox-theme gotham-theme google-this god-mode git-timemachine flymake-shellcheck flycheck-joker flycheck-clj-kondo flx flatui-theme fill-column-indicator eyebrowse exec-path-from-shell elpy dockerfile-mode diminish darktooth-theme company-tern company-anaconda clojure-mode cider-eval-sexp-fu challenger-deep-theme browse-kill-ring badger-theme apropospriate-theme ample-theme all-the-icons adoc-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-repl-init-code "cerber.oauth2.standalone.system/go")
     (cider-repl-init-code "mount/start")
     (cider-repl-init-code . "mount/start")
     (cider-ns-refresh-after-fn . "mount/start")
     (cider-ns-refresh-before-fn . "mount/stop")
     (cider-repl-init-code "(cerber.oauth2.standalone.system/go)")
     (cider-repl-init-code "(go)")
     (cider-ns-refresh-after-fn . "cerber.oauth2.standalone.system/start")
     (cider-ns-refresh-before-fn . "cerber.oauth2.standalone.system/stop")
     (cljr-libspec-whitelist "^thanks.spec" "^integrant.repl$" "^day8.re-frame.http-fx$" "^tick.locale.*$" "^thanks.frontend.*$" "^spell-spec.expound$" "^duct.core.resource*")
     (cljr-libspec-whitelist "^thanks.spec" "^integrant.repl$" "^day8.re-frame.http-fx$" "^tick.locale.*$" "^thanks.frontend.*$" "^spell-spec.expound$")
     (cljr-libspec-whitelist "^thanks.spec" "^integrant.repl$" "^day8.re-frame.http-fx$" "^tick.locale.*$" "^thanks.frontend.*$" "^thanks.api.oauth.stores$")
     (cljr-libspec-whitelist "^thanks.spec" "^integrant.repl$" "^day8.re-frame.http-fx$" "^tick.locale.*$" "^thanks.frontend.*$")
     (cljr-auto-clean-ns . t)
     (cljr-favor-prefix-notation)
     (cljr-libspec-whitelist "^thanks.spec" "^integrant.repl$")
     (cljr-libspec-whitelist "^thanks.spec" "^integrant.repl")
     (cider-repl-init-code "(do (dev) (go))")
     (cljr-libspec-whitelist "^thanks.spec")
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (auto-fill-mode t)
     (cider-clojure-cli-global-options . "-A:dev:test:frontend:dev/frontend")
     (cider-cljs-repl-types
      (edge "(do (require 'dev-extras) ((resolve 'dev-extras/cljs-repl) \"app\"))"))
     (cider-default-cljs-repl . edge)
     (cider-clojure-cli-global-options . "-A:dev:test:frontend:dev/build")
     (cider-repl-init-code "(dev)")
     (cider-ns-refresh-after-fn . "dev/resume")
     (cider-ns-refresh-before-fn . "dev/suspend")
     (cider-ns-refresh-after-fn . "mount.core/start")
     (cider-ns-refresh-before-fn . "mount.core/stop"))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#ecf0f1")
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))))
 '(vc-annotate-very-old-color "#0a74b9"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
