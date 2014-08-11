(autoload 'org-mode "org.el" "Org Mode!" t)

(add-hook 'org-mode-hook  (lambda ()
                            (flyspell-mode 1)    
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

(setq org-capture-templates
      '(("n" "Note" plain (file (concat org-directory "/notes.org"))
         "- %?\n  %i\n")))
