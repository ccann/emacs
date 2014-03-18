;;; init.el --- My init file for emacs

;;; Commentary: 
;;; author: ccann

;;; Code: 

;;;;;;;;;;;;;;;;;;;;;
;;;;   Startup   ;;;;
;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(idle-highlight-mode
                      find-file-in-project
                      smex
                      ido-ubiquitous
                      paredit
                      magit
                      rainbow-mode
                      rainbow-delimiters
                      auto-complete
                      dired-details+
                      lua-mode
                      clojure-mode
                      cider
                      multi-web-mode
                      ess
                      auto-complete
                      jedi
                      align-cljlet
                      gnuplot)
  "List of packages to ensure are installed at startup.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/dotfiles/lisp/")
(add-to-list 'load-path "~/dev/badger-theme/")

(load "badger-theme.el")
(load "settings.el")
(load "keybindings.el")
(load "mode-hooks.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   Added by Emacs   ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (badger)))
 '(custom-safe-themes (quote ("75f1b91fd136530b976c3ee12ce05a53e3167af06f84a70d400af015aa042bca" default)))
 '(org-agenda-files (quote ("~/docs/agendas/"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

