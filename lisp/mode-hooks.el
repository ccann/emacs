;;; mode-hooks.el

;;; Commentary:

;;; Code:
;;;###autoload

(progn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   LaTeX  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (autoload 'tex-mode "tex-site" "AucTeX" t)
  (eval-after-load "pretty-mode" '(pretty-mode 1))

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (progn
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;   Emacs Lisp  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (rainbow-mode 1)
                                    (paredit-mode 1)
                                    (rainbow-delimiters-mode 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;     PYTHON     ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (autoload 'jedi:setup "jedi" nil t)
  (setq-default python-indent-guess-indent-offset nil)
  (setq-default python-indent-offset 4)
  (add-hook 'python-mode-hook (lambda ()
                                (jedi:setup)
                                (auto-complete-mode 1)
                                (rainbow-delimiters-mode 1)))
  
  (setq jedi:complete-on-dot t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      Org       ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (autoload 'org-mode "org.el" "Org Mode!" t)

  (add-hook 'org-mode-hook  (lambda ()
                              (flyspell-mode 1)    
                              (auto-fill-mode 1)    
                              (visual-line-mode 1)
                              (org-indent-mode 1)))

  (setq org-tags-column 85)
  (setq org-latex-to-pdf-process (list "latexmk -f -pdf"))
  (setq org-hide-leading-stars t)
  (setq org-fontify-done-headline nil)

  ;; Make Org-mode use evince in linux to open PDFs
  (if (not (eq system-type 'darwin))
      (add-hook 'org-mode-hook
                (lambda ()
                  (delete '("\\.pdf\\'" . default) org-file-apps)
                  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Clojure     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'clojure-mode-hook (lambda ()
                                 (paredit-mode 1)
                                 (rainbow-delimiters-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;
;;;     Web    ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-css-indent-offset 4)
                             (setq web-mode-code-indent-offset 4)))


;;;;;;;;;;;;;;;;;;;;;;;
;;;;      R      ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
  (autoload 'ess-eldoc "ess-eldoc" t)
  (autoload 'R "ess-site.el" "Emacs Speaks Statistics" t)
  (autoload 'R-mode "ess-site.el" "Emacs Speaks Statistics" t)
  (autoload 'r-mode "ess-site.el" "Emacs Speaks Statistics" t)

  (add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
  (add-to-list 'auto-mode-alist '("\\.r$" . R-mode))

  (setq-default inferior-R-program-name "R")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-eval-visibly-p nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;      LUA      ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (add-hook 'lua-mode-hook (lambda ()
                             (rainbow-mode 1)))


  ;; impcore -> scheme binding
  (add-to-list 'auto-mode-alist '("\\.ic\\'" . scheme-mode)))
(provide 'mode-hooks)
;;; mode-hooks ends here
