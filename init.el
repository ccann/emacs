;;; init.el --- My init file for emacs

;;; Commentary: 
;;; author: ccann

;;; Code: 

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(idle-highlight-mode
                      find-file-in-project
                      elpy
                      jedi                   
                      org
                      smex
                      ido-ubiquitous
                      paredit
                      magit
                      rainbow-delimiters
                      auto-complete
                      company
                      dired-details+
                      lua-mode
                      clojure-mode           
                      cider                  
                      ess
                      markdown-mode
                      smart-mode-line
                      web-mode
                      auto-complete
                      flymake
                      align-cljlet
                      pretty-mode
                      gnuplot
                      fill-column-indicator
                      ido-vertical-mode
                      indent-guide
                      flx-ido
                      flatui-theme
                      badger-theme
                      htmlize
                      )
  "List of packages to ensure are installed at startup.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Keep emacs custom-settings in separate file, for fuck's sake.
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path "~/dev/qichat-mode")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/modes/")
(require 'qichat-mode)

(load "config.el")
(load "keybindings.el")
(load "functions.el")

(setq my-themes '(flatui badger))
(cycle-my-theme)

(defun ccc/configure-all-custom-modes ()
  (mapcar (lambda (mode-file-name) (load mode-file-name))
          (directory-files "~/.emacs.d/modes/" nil ".el")))

(ccc/configure-all-custom-modes)

;;; init.el ends here