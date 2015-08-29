;;; init.el --- My init file for emacs

;;; Commentary:
;;; author: ccann

;;; Code:

(setq load-prefer-newer t)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
   (require 'use-package))
(defvar use-package-verbose t)
(require 'bind-key)
(require 'diminish)

(defconst ccann/is-osx (eq system-type 'darwin))

;;;;;;;;;;;;;;;
;; Modifiers ;;
;;;;;;;;;;;;;;;
(when ccann/is-osx
  (setq mac-command-modifier 'control)
  ;; (setq mac-control-modifier 'meta)
  )

(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy nil) ;; Standard Windows behaviour
(cua-mode 1)
(setq cua-prefix-override-inhibit-delay 0.05)

;;;;;;;;;;
;; libs ;;
;;;;;;;;;;
(use-package list-utils)
(use-package popwin
  :defer t
  :config
  (popwin-mode 1))

;;;;;;;;;;;;;
;; Loaders ;;
;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(load (expand-file-name "functions.el" user-emacs-directory))

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;
(use-package idle-highlight-mode
  :defer t
  :init (setq idle-highlight-idle-time 1.0))

(use-package smart-tab
  :init (bind-key "<tab>" 'hippie-expand read-expression-map))

;; (use-package subword-mode :defer t :diminish subword-mode)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))  ;; smart meta-x (use IDO in minibuffer)

(use-package ido
  :demand t
  :bind (("C-x b" . ido-switch-buffer))
  :init
  (setq ido-create-new-buffer 'always  ;; don't confirm when creating new buffers
        ido-enable-flex-matching t  ;; fuzzy matching
        ido-everywhere t  ;; tbd
        ido-case-fold t)  ;; ignore case

  (use-package ido-ubiquitous :config (ido-ubiquitous-mode 1))
  (use-package flx-ido :config (flx-ido-mode 1))
  (use-package ido-vertical-mode :config (ido-vertical-mode 1))
  :config (ido-mode 1))
  
(use-package magit
  :diminish magit-auto-revert-mode
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  :bind (("<f10>" . magit-status))
  :config
  (when ccann/is-osx
    (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")))

(use-package rainbow-delimiters :defer t)
(use-package rainbow-mode :defer t)
(use-package company
  :init
  (use-package company-anaconda
    :diminish anaconda-mode
    :config (add-to-list 'company-backends 'company-anaconda))
  :defer t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.4)
  (setq company-minimum-prefix-length 3))

(use-package dired-details+ :defer t)

(use-package uniquify :init (setq uniquify-buffer-name-style 'forward))

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))


(use-package smart-mode-line
  :init
  (setq sml/mule-info nil)
  (setq sml/numbers-separator ":")
  (setq sml/show-remote nil)
  (setq sml/modified-char " x ")
  :config
  (sml/setup))

(use-package zone
  :defer t
  :config (zone-when-idle 300))

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  ;; Custom fringe indicator (circle)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
	      #b00000000
	      #b00000000
	      #b00000000
	      #b00000000
	      #b00000000
	      #b00000000
	      #b00011100
	      #b00111110
	      #b00111110
	      #b00111110
	      #b00011100
	      #b00000000
	      #b00000000
	      #b00000000
	      #b00000000
	      #b01111111)))

    (flycheck-define-error-level 'error
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-error)
    
    (flycheck-define-error-level 'warning
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-warning)
    
    (flycheck-define-error-level 'info
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'my-flycheck-fringe-indicator
      :fringe-face 'flycheck-fringe-info))


(use-package fill-column-indicator :disabled t)

(use-package indent-guide :disabled t)

(use-package highlight-symbol
  :init
  (setq highlight-symbol-on-navigation-p t)
  (setq hi-lock-auto-select-face t) ;; when non-nil cycle through faces in hi-lock-faces-defaults instead of prompting
  :bind
  (("C-h s" . highlight-symbol))
  :config
  (highlight-symbol-mode 1))
  
(use-package hlinum :defer t)
(use-package nyan-mode :config (nyan-mode 1))

;;;;;;;;;;;;;;;;;;
;; markup modes ;;
;;;;;;;;;;;;;;;;;;

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; fontify source blocks
  (setq org-src-fontify-natively t)
  ;; export to html5
  (setq org-html-doctype "html5")
  ;; use new fancy element types if you want
  (setq org-html-html5-fancy t)
  (setq org-html-postamble nil)
  (setq org-hide-leading-stars t)
  (setq org-capture-templates
        '(("v" "Vessel Watcher" entry (file+headline (concat org-directory "/vessel-watcher.org") "Tasks")
           "* TODO %?\n  %T\n  %i\n")
          ("c" "Conflux" entry (file+headline (concat org-directory "/conflux.org") "Tasks")
           "* TODO %?\n  %T\n  %i\n")
          ("m" "Miscellaneous" entry (file+headline (concat org-directory "/misc.org") "Tasks")
           "* TODO %?\n  %T\n  %i\n")
          ("n" "Notes" entry (file+headline (concat org-directory "/notes.org") "Notes")
           "* %?\n  %T\n")))


  :bind
  (("C-c C-r C-b" . org-bibtex-read-buffer)
   ("C-c C-w" . org-bibtex-write)
   ("C-c a" . org-agenda)
   ("<f8>" . org-capture)
   ))
;; (add-hook 'org-mode-hook  (lambda ()
;;                             (flyspell-mode 1)    
;;                             (visual-line-mode 1)
;;                             (org-indent-mode 1)
;;                             (fci-mode -1)
;;                             (auto-fill-mode 1)))

;; (setq org-tags-column 85)
;; (setq org-latex-to-pdf-process (list "latexmk -f -pdf"))
;; (setq org-fontify-done-headline nil)

;; ;; Make Org-mode use evince in linux to open PDFs
;; (if (not (eq system-type 'darwin))
;;     (add-hook 'org-mode-hook
;;               (lambda ()
;;                 (delete '("\\.pdf\\'" . default) org-file-apps)
;;                 (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))))


(use-package markdown-mode :defer t)

(use-package auctex
  :bind  (("C-c k" . compile)))
(use-package auctex-latexmk :defer t)
;; this takes forever to require...
;; (require 'auctex-latexmk)

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (progn
;;               (auctex-latexmk-setup)
;;               (visual-line-mode 1)
;;               (turn-on-reftex)
;;               (TeX-source-correlate-mode 1)
;;               (TeX-PDF-mode 1)
;;               (TeX-auto-save 1)
;;               (TeX-parse-self 1)
;;               (reftex-plug-into-AUCTeX 1)
;;               (flyspell-mode 0)
;;               (autoload 'pretty-mode "pretty-mode.el" "Pretty Mode" t)
;;               (TeX-source-correlate-start-server 1))))


;; ;; link auctex to Preview in OSX, Evince in linux
;; (setq TeX-view-program-list '(("Preview" "open /Applications/Preview.app %o"
;;                                "Evince" "evince --page-index=%(outpage) %o")))
;; (if (eq system-type 'darwin)
;;     (setq TeX-view-program-selection '((output-pdf "Preview")))
;;   (setq TeX-view-program-selection '((output-pdf "Evince"))))



;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 80)
(show-paren-mode 1)  ;; visualize matching parens
(global-hl-line-mode 1)

(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :init
  (setq sp-override-key-bindings '(("C-<right>" . nil)
                                   ("C-<left>" . nil)
                                   ("C-)" . sp-forward-slurp-sexp)
                                   ("M-<backspace>" . nil)
                                   ("C-(" . sp-forward-barf-sexp)))
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))
  
(use-package ess :defer t)

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq-default python-indent-guess-indent-offset nil)
  (setq-default python-indent-offset 4)
  (setq python-fill-docstring-style 'pep-257-nn)
  (setq python-check-command "flake8")
  
  (use-package elpy
    :defer t
    :diminish elpy-mode
    :init
    (setq elpy-rpc-backend "jedi")
    (setq elpy-modules '(elpy-module-company
                         elpy-module-eldoc
                         elpy-module-sane-defaults))
    (when ccann/is-osx
      (setq elpy-rpc-python-command "/usr/local/bin/python")))

  (use-package jedi
    :defer t
    :init (setq jedi:complete-on-dot t)
    :bind (("C-c d" . jedi:show-doc)))
  
  (add-hook 'python-mode-hook (lambda ()
                                (elpy-enable)
                                (elpy-mode)
                                (subword-mode 1)
                                (linum-mode 1)
                                (company-mode 1)
                                (rainbow-delimiters-mode 1)
                                (elpy-use-ipython)
                                (fci-mode 0)
                                (flycheck-mode 1)
                                (anaconda-mode 0) ;; broken?
                                (auto-fill-mode 0))))
  


(use-package lua-mode :defer t)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (use-package cider
    :defer t
    :init
    (setq nrepl-log-messages t) ;; Log communication with the nREPL server
    (setq cider-repl-display-in-current-window t) ;; C-c C-z switch to the CIDER REPL buffer
    (setq cider-prompt-save-file-on-load nil))
  (use-package clj-refactor :defer t :diminish clj-refactor-mode)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (cider-mode 1)
                                 (smartparens-mode 1)
                                 (rainbow-delimiters-mode 1)
                                 ;; (paredit-mode 0)
                                 (linum-mode 1)
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1) ; for adding require/use/import
                                 (cljr-add-keybindings-with-prefix "C-c C-m")
                                 (eldoc-mode 1)
                                 (idle-highlight-mode 1)
                                 (company-mode 1))))



(use-package emacs-lisp
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (flycheck-mode 1)
                                    (linum-mode 1)
                                    (company-mode 1)
                                    (smartparens-mode 1)
                                    (rainbow-delimiters-mode 1))))


(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))


(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :diminish projectile-mode
  :config
  (projectile-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;
;; Display Settings ;;
;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode 1)
(blink-cursor-mode 1)
(set-fringe-mode '(1 . 0)) ;; turn off right fringe
(setq visible-bell nil) ;; if visible-bell nil, ring-bell-function is alarm
(setq ring-bell-function `(lambda () )) ;; empty alarm function. voila.
(setq inhibit-startup-screen t) ;; turn off splash screen
(setq ns-use-srgb-colorspace t)
(if ccann/is-osx
       (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
  (progn
    (menu-bar-mode 0)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))


(setq window-combination-resize t) ;; golden ration better
(setq x-underline-at-descent-line t) ;; draw underline lower

;; (setq initial-frame-alist ;; I dont think this does anything ... at least on OSX
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))



;; prevent linum-mode and text-scale-adjust from fucking each other
(add-hook 'linum-mode-hook
          (lambda ()
            (set-face-attribute 'linum nil :height 100)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Buffers and Navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-major-mode 'text-mode
      initial-scratch-message "")

(use-package ibuffer
  :defer t
  :init
  (setq ibuffer-shrink-to-minimum-size t)
  (setq ibuffer-always-show-last-buffer nil)
  (setq ibuffer-sorting-mode 'recency)
  (setq ibuffer-use-header-line t))

;; activate SHIFT + arrow keys for window moving
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(global-auto-revert-mode t) ;; automatically reload changed buffers
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil) ;; disallow tab insertion

;; configure smooth scrolling
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; configure clipoard
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t) ;; Mouse-2 inserts text at point, not click location

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
(global-set-key (kbd "C-S-n") 'scroll-up)
(global-set-key (kbd "C-S-p") 'scroll-down)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "<f12>") 'ibuffer)
(global-set-key (kbd "<f9>") 'cycle-my-theme)
(global-set-key (kbd "C-;") 'endless/comment-line)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)

(use-package hydra :defer t)

(use-package key-chord
  :init
  (key-chord-mode 1)
  (key-chord-define-global "sf" 'save-buffer)
  ;; (key-chord-define-global "jf" 'ido-find-file)
  (key-chord-define-global "jf" 'projectile-find-file)
  (key-chord-define-global "sp" 'projectile-switch-project)
  (key-chord-define-global "sb" 'ido-switch-buffer))


;;;;;;;;;;;;
;; themes ;;
;;;;;;;;;;;;
(use-package flatui-theme :defer t)
(use-package zenburn-theme :defer t)
(use-package badger-theme :defer t)
(use-package gotham-theme :defer t)
(use-package darktooth-theme :defer t)
(use-package material-theme :defer t)
(defvar curr-theme nil)
(defvar my-themes '(flatui darktooth))
(cycle-my-theme)


;;; init.el ends here
