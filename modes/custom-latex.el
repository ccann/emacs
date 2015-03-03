;; (autoload 'tex-mode "tex-site" "AucTeX" t)
;;(eval-after-load "pretty-mode" '(pretty-mode 1))

;; this takes forever to require...
;; (require 'auctex-latexmk)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (progn
              (auctex-latexmk-setup)
              (visual-line-mode 1)
              (turn-on-reftex)
              (TeX-source-correlate-mode 1)
              (TeX-PDF-mode 1)
              (TeX-auto-save 1)
              (TeX-parse-self 1)
              (reftex-plug-into-AUCTeX 1)
              (flyspell-mode 0)
              (autoload 'pretty-mode "pretty-mode.el" "Pretty Mode" t)
              (TeX-source-correlate-start-server 1))))


;; link auctex to Preview in OSX, Evince in linux
(setq TeX-view-program-list '(("Preview" "open /Applications/Preview.app %o"
                               "Evince" "evince --page-index=%(outpage) %o")))
(if (eq system-type 'darwin)
    (setq TeX-view-program-selection '((output-pdf "Preview")))
  (setq TeX-view-program-selection '((output-pdf "Evince"))))
