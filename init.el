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
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(idle-highlight-mode
                      find-file-in-project
                      org
                      smex
                      ido-ubiquitous
                      paredit
                      magit
                      rainbow-delimiters
                      auto-complete
                      dired-details+
                      lua-mode
                      clojure-mode            ;; :clojure
                      cider                   ;; :clojure
                      ess
                      markdown-mode
                      smart-mode-line
                      web-mode
                      auto-complete
                      jedi                    ;; :python
                      flymake
                      elpy                    ;; :python
                      align-cljlet
                      pretty-mode
                      gnuplot)
  "List of packages to ensure are installed at startup.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Keep emacs custom-settings in separate file, for fuck's sake.
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load custom-file)

;; set the PYTHONPATH to be what the shell has
(let ((path-from-shell (shell-command-to-string "echo $PYTHONPATH")))
  (setenv "PYTHONPATH" (mapconcat 'identity (split-string path-from-shell) ":")
    ))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/dev/qichat-mode")

(require 'settings)
(require 'core-extensions)
(require 'mode-hooks)
(require 'keybindings)
(require 'functions)
(require 'qichat-mode)

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/badger-theme")
(setq my-themes '(flatui badger))
(cycle-my-theme)

(setq python-check-command "flake8")

;;; init.el ends here

