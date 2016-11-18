;;; init.el --- My init file for emacs

;;; Commentary:
;;; LaTeX installation on OSX 10.11.1
;;;  $ brew up
;;;  $ brew install caskroom/cask/brew-cask
;;;  $ brew cask install mactex
;;; # add /usr/texbin to PATH
;;;  $ brew install latex-mk
;;;  $ brew install auctex

;; author: ccann

;;; Code:

(setq load-prefer-newer t)

;; this seemingly has no effect...
;; (let ((display-table (or standard-display-table (make-display-table))))
;;   (set-display-table-slot display-table 'vertical-border (make-glyph-code ?┃))
;;   (setq standard-display-table display-table))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; some wizard online concocted this
(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish)

(defconst ccann/is-osx (eq system-type 'darwin))

;; (use-package spaceline
;;   :init
;;   (require 'spaceline-config)
;;   (setq powerline-default-separator 'wave)
;;   (setq powerline-height 20)
;;   (setq powerline-raw " ")
;;   (setq ns-use-srgb-colorspace nil)
;;   :ensure t
;;   :config (spaceline-emacs-theme))


;;;;;;;;;;;;;;
;; Security ;;
;;;;;;;;;;;;;;

;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html

;; (setq tls-checktrust t) ;; always

;; (let ((trustfile
;;        (replace-regexp-in-string
;;         "\\\\" "/"
;;         (replace-regexp-in-string
;;          "\n" ""
;;          (shell-command-to-string "python -m certifi")))))
;;   (setq tls-program
;;         (list
;;          (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
;;                  (if (eq window-system 'w32) ".exe" "") trustfile)))
;;   (setq gnutls-verify-error t)
;;   (setq gnutls-trustfiles (list trustfile)))


;;;;;;;;;;;;;;
; Modifiers ;;
;;;;;;;;;;;;;;

(use-package god-mode
  :config
  ;; (god-mode-all)
  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (bind-key "i" 'god-local-mode god-local-mode-map))

;; (use-package evil
;;   :ensure t
;;   :config  (evil-mode 1)
;;   )

;; (use-package evil-escape
;;   :ensure t
;;   :init (setq-default evil-escape-key-sequence "fd")
;;   :config (evil-escape-mode 1))

;; (use-package evil-tutor :ensure t)

;;;;;;;;;
; libs ;;
;;;;;;;;;
(use-package list-utils)
(use-package popwin
  :defer t
  :config (popwin-mode 1))

;;;;;;;;;;;;
; Loaders ;;
;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(load (expand-file-name "functions.el" user-emacs-directory))

(add-to-list 'auto-mode-alist '("\\.cql\\'" . sql-mode))

;;;;;;;;;;;;;
; Packages ;;
;;;;;;;;;;;;;
(use-package idle-highlight-mode
  :defer t
  :init (setq idle-highlight-idle-time 0.3))

;; indent unless point is at the end of a symbol
;; (use-package smart-tab
;;   :init (bind-key "<tab>" 'hippie-expand read-expression-map))

(use-package exec-path-from-shell
  :defer 2
  :config
  (when ccann/is-osx
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs (ccann/get-envs "~/.profile"))
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(use-package smex
  :bind (("M-x" . smex))
  :config (smex-initialize))  ; smart meta-x (use IDO in minibuffer)

(use-package ido
  :demand t
  :bind (("C-x b" . ido-switch-buffer))
  :init
  (setq ido-create-new-buffer 'always  ; don't confirm when creating new buffers
        ido-enable-flex-matching t     ; fuzzy matching
        ido-everywhere t  ; tbd
        ido-case-fold t)  ; ignore case
  :config (ido-mode 1))

(use-package ido-ubiquitous  :config (ido-ubiquitous-mode 1))
(use-package flx-ido  :config (flx-ido-mode 1))
(use-package ido-vertical-mode  :config (ido-vertical-mode 1))

(use-package magit
  :diminish magit-auto-revert-mode
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-display-buffer-function  #'magit-display-buffer-fullframe-status-v1)
  :bind (("<f10>" . magit-status))
  :config
  (when ccann/is-osx
    (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")))

(use-package rainbow-delimiters :defer t)
(use-package rainbow-mode :defer t)

(use-package company-anaconda
  :defer t
  :diminish anaconda-mode)

(use-package company
  :defer 2
  :init
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3)
  (global-set-key (kbd "<S-tab>") #'company-indent-or-complete-common)
  (setq tab-always-indent t) ;; set this to nil if you want `indent-for-tab-command` instead
  :diminish company-mode
  :config 
  (add-to-list 'company-backends 'company-anaconda)
  (global-company-mode))

(use-package dired-details+ :defer t)

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

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  ; Custom fringe indicator (circle)
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
  (setq hi-lock-auto-select-face t) ; when non-nil cycle through faces in hi-lock-faces-defaults instead of prompting
  :bind
  (("C-h s" . highlight-symbol))
  :config
  (highlight-symbol-mode 1))
  
;; (use-package hlinum :config (hlinum-activate))
(use-package nyan-mode
  :config (nyan-mode -1))

;;;;;;;;;;;;;;;;;;
;; markup modes ;;
;;;;;;;;;;;;;;;;;;


(use-package ob-ipython
  :ensure t)

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-directory "~/Dropbox (Personal)/org")
  (use-package htmlize)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'auto-fill-mode)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (shell . t)
     (python . t)))
  ;; the following jekyll-boostrap integration depends on ~/dev/ccann.github.io
  ;; and ~/blog existing, see below.
  (setq org-hide-emphasis-markers nil)
  
  (setq org-publish-project-alist
        '(("org-ccann"
           :base-directory "~/blog"  ;; Path to your org files.
           :base-extension "org" 
           :publishing-directory "~/dev/ccann.github.io/_posts" ;; Path to your Jekyll project.
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t)
          ("org-static-ccann"
           :base-directory "~/blog/images"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
           :publishing-directory "~/dev/ccann.github.io/assets"
           :recursive t
           :publishing-function org-publish-attachment)
          ("blog" :components ("org-ccann" "org-static-ccann"))))

  (setq org-default-notes-file (concat org-directory "/notes.org")
        org-src-fontify-natively t ; fontify source blocks
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-postamble nil
        org-hide-leading-stars t
        org-tags-column 85
        org-latex-to-pdf-process (list "latexmk -f -pdf")
        org-fontify-done-headline nil)

  (setq org-capture-templates
        '(("l" "Programming Language Note" entry
           (file+headline (concat org-directory "/notes.org") "Languages")
           "* %?")
          ("d" "Database Note" entry
           (file+headline (concat org-directory "/notes.org") "Databases")
           "* %?")))
        
  ;; Make Org-mode use evince in linux to open PDFs
  (if (not ccann/is-osx)
      (add-hook 'org-mode-hook
                (lambda ()
                  (delete '("\\.pdf\\'" . default) org-file-apps)
                  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))))

  :bind
  (("<f8>" . org-capture)))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (if ccann/is-osx
      (setq TeX-view-program-selection '((output-pdf "Preview")))
    (setq TeX-view-program-selection '((output-pdf "Evince"))))
  :init
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'auctex-latexmk-setup)
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-view-program-list '(("Preview" "open /Applications/Preview.app %o"
                                 "Evince" "evince --page-index=%(outpage) %o")))
  (setq-default TeX-master nil))


(use-package auctex-latexmk
  :defer t
  :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :commands auctex-latexmk-setup)

(use-package reftex
  :defer t
  :commands turn-on-reftex
  :init (setq reftex-plug-into-AUCTeX t))

;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 89)
(show-paren-mode 1)
(global-hl-line-mode 1)

(use-package lispy
  :ensure t)

(use-package ess :defer t)

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :init 
  (add-hook 'ruby-mode-hook #'linum-mode)
  (add-hook 'ruby-mode-hook #'idle-highlight-mode))

(use-package python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook #'elpy-mode)
  (add-hook 'python-mode-hook #'elpy-enable)
  (add-hook 'python-mode-hook (lambda () (elpy-use-ipython "ipython")))
  (add-hook 'python-mode-hook #'subword-mode)
  (add-hook 'python-mode-hook #'linum-mode)
  ;; (add-hook 'python-mode-hook #'rainbow-delimiters-mode)
  
  (add-hook 'python-mode-hook #'flycheck-mode)
  (add-hook 'python-mode-hook #'idle-highlight-mode)
  (add-hook 'python-mode-hook #'eldoc-mode)
  
  (setq-default python-indent-guess-indent-offset nil
                python-indent-offset 4)
  (setq python-fill-docstring-style 'pep-257-nn
        python-check-command "flake8"))

(use-package elpy
  :ensure t
  :init
  (fringe-mode '(10 . 0))
  (setq elpy-rpc-backend "jedi")
  (setq elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-sane-defaults
                       elpy-module-pyvenv))
  (when ccann/is-osx
    (setq elpy-rpc-python-command "/usr/local/bin/python3")))

(use-package jedi
  :defer t
  :init (setq jedi:complete-on-dot t)
  :bind (("C-c d" . jedi:show-doc)))

(use-package lua-mode :defer t)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :diminish (subword-mode)
  :config
  (define-clojure-indent
    ; compojure
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    ; metosin
    (defapi 'defun)
    (swaggered 'defun)
    (swagger-docs 2)
    (GET* 2)
    (POST* 2)
    (PUT* 2)
    (DELETE* 2)
    (HEAD* 2)
    (ANY* 2))
  :init
  (use-package slamhound :defer t )
  (setq cljr-suppress-middleware-warnings t)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'linum-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'idle-highlight-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (hi-lock-mode -1)))
  (add-hook 'clojure-mode-hook #'lispy-mode))

(use-package cider
  :ensure t
  :defer t
  :init
  (use-package cider-eval-sexp-fu)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (hi-lock-mode -1)))
  :config
  (setq nrepl-log-messages t                    ; log communication with the nREPL server
        cider-repl-display-in-current-window t 
        cider-repl-use-clojure-font-lock t
        cider-prompt-save-file-on-load nil
        cider-prompt-for-symbol nil
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            ; hide *nrepl-connection* and *nrepl-server*
        cider-overlays-use-font-lock nil
        nrepl-prompt-to-kill-server-buffer-on-quit nil)
  (cider-repl-toggle-pretty-printing))

(use-package clj-refactor
  :ensure t
  :defer t
  :pin melpa-stable
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'linum-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'emacs-lisp-mode-hook #'idle-highlight-mode)
(diminish 'auto-revert-mode)


(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :init
  (setq js-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (add-hook 'web-mode-hook #'linum-mode)
  (add-hook 'web-mode-hook #'idle-highlight-mode))


(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :diminish projectile-mode
  :config
  (setq shell-file-name "/bin/sh")
  (projectile-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;
;; Display Settings ;;
;;;;;;;;;;;;;;;;;;;;;;
(column-number-mode 1)
(blink-cursor-mode 1)
(set-fringe-mode '(1 . 0))
(setq visible-bell nil) ; if visible-bell nil, ring-bell-function is alarm
(setq ring-bell-function `(lambda () )) ; empty alarm function. voila.
(setq inhibit-startup-screen t) ; turn off splash screen
(setq ns-use-srgb-colorspace t)
(if ccann/is-osx
    (set-face-attribute 'default nil :weight 'normal :font "Office Code Pro-13")
  (progn
    (menu-bar-mode 0)
    (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))


(setq window-combination-resize t) ; golden ration better
(setq x-underline-at-descent-line t) ; draw underline lower

(setq initial-frame-alist
      '((menu-bar-lines . 0)   ; not using menu-bar, default is 1 line
        (tool-bar-lines . 0))) ; not using tool-bar, default is 1 line

; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

; prevent linum-mode and text-scale-adjust from fucking each other, not a perfect solution
(add-hook 'linum-mode-hook
          (lambda ()
            (set-face-attribute 'linum nil :height 100)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;Buffers and Navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-major-mode 'text-mode initial-scratch-message "")

(use-package ibuffer
  :defer t
  :init
  (setq ibuffer-shrink-to-minimum-size t)
  (setq ibuffer-always-show-last-buffer nil)
  (setq ibuffer-sorting-mode 'recency)
  (setq ibuffer-use-header-line t))

; activate SHIFT + arrow keys for window moving
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;;;;;;;
; Misc ;;
;;;;;;;;;
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(global-auto-revert-mode t) ; automatically reload changed buffers
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil) ; disallow tab insertion

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1))

(setq mouse-wheel-scroll-amount '(2)) ;; n lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse


; configure clipoard
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t) ; Mouse-2 inserts text at point, not click location

(delete-selection-mode 1)

(use-package which-key
  :ensure t
  :diminish (which-key-mode)
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support))

(use-package visual-regexp
  :defer t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))
  
(setq scroll-error-top-bottom t)
;;;;;;;;;;;;;;;;
; Keybindings ;;
;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
(global-set-key (kbd "C-S-n") (lambda () (interactive) (scroll-up 8) (next-line 8)))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (scroll-down 8) (previous-line 8)))
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c N") 'find-notes-file)
(global-set-key (kbd "<f12>") 'ibuffer)
(global-set-key (kbd "<f9>") 'cycle-my-theme)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)

(use-package hydra :defer t)

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "sz" 'save-buffer)
  (key-chord-define-global "jf" 'projectile-find-file)
  (key-chord-define-global "jp" 'projectile-switch-project)
  (key-chord-define-global "fb" 'ido-switch-buffer)
  ;; (key-chord-define-global "mk" 'multiple-cursors-hydra/body)
  ;; (key-chord-define-global "fd" 'god-local-mode)
)


;;;;;;;;;;;
; themes ;;
;;;;;;;;;;;
(use-package flatui-theme :defer t )
(use-package zenburn-theme :defer t)
(use-package badger-theme :defer t)
(use-package gotham-theme :defer t)
(use-package darktooth-theme :defer t)
(use-package material-theme :defer t)
(use-package metalheart-theme :defer t)
(use-package apropospriate-theme :defer t)

(defvar curr-theme nil)
(defvar my-themes '(flatui apropospriate-light darktooth metalheart apropospriate-dark))
(cycle-my-theme)



;; init.el ends here

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
