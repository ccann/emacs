;;; init.el --- My init file for emacs

;;; Commentary:
;;; author: ccann

;;; Code:

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
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
                      rainbow-mode
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
                      flycheck
                      auctex
                      auctex-latexmk
                      align-cljlet
                      pretty-mode
                      gnuplot
                      fill-column-indicator
                      ido-vertical-mode
                      indent-guide
                      flx-ido
                      htmlize
                      popwin
                      highlight-symbol
                      projectile
                      company-anaconda
                      hydra
                      flatui-theme
                      zenburn-theme
                      badger-theme
                      gotham-theme
                      darktooth-theme
                      material-theme
                      ;; evil-mode
                      ;; evil-leader
                      ;; evil-tutor
                      ;; key-chord-mode
                      )
  "List of packages to ensure are installed at startup.")

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Load Paths

;; Keep emacs custom-settings in separate file, for fuck's sake.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/lisp")
(defvar user-config-file "~/.emacs.d/lisp/config.el")
(load "config.el")
(load "keybindings.el")
(load "functions.el")

(defvar emacs-d-modes "~/.emacs.d/modes/")
(add-to-list 'load-path emacs-d-modes)

;; configure all custom modes in emacs.d/modes
(mapcar (lambda (mode-file-name) (load mode-file-name))
        (directory-files emacs-d-modes nil ".el"))

;; Theme

(setq my-themes '(darktooth flatui material gotham badger))
(load-theme (car my-themes) t)
(sml/apply-theme 'respectful)
(underline-straight)
(defvar curr-theme (pop my-themes))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))


;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)


;;; init.el ends here
