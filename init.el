;;; init.el --- My init file for emacs

;;; Commentary: 
;;; author: ccann

;;; Code: 

;;;;;;;;;;;;;;;;;;;;;
;;;;   Startup   ;;;;
;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
		         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(idle-highlight-mode
                      find-file-in-project
                      smex
                      ido-ubiquitous
                      paredit
                      magit
                      smart-mode-line
                      rainbow-mode
                      rainbow-delimiters
                      auto-complete
                      dired-details+
                      lua-mode
                      clojure-mode
                      cider
                      web-mode
		      elpy
                      ess
                      auto-complete
                      jedi
                      align-cljlet
                      gnuplot)
  "List of packages to ensure are installed at startup.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq custom-file "~/.emacs.d/lisp/custom.el")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/dev/badger-theme")

(require 'settings)
(require 'core-extensions)
(require 'keybindings)
(require 'mode-hooks)

(load-theme 'badger t)

;;; init.el ends here

