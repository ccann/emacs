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

(setq-default warning-minimum-level :emergency)
;; (setq load-prefer-newer t)

;; this seemingly has no effect...
;; (let ((display-table (or standard-display-table (make-display-table))))
;;   (set-display-table-slot display-table 'vertical-border (make-glyph-code ?┃))
;;   (setq standard-display-table display-table))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; some wizard online concocted this
(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile 
  (require 'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-ensure t)
  (require 'bind-key)
  (require 'diminish))


(defconst ccann/is-osx (eq system-type 'darwin))

;; libs
(use-package list-utils)
(use-package hydra :defer t)
(use-package popwin :defer t :config (popwin-mode 1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(load (expand-file-name "functions.el" user-emacs-directory))
(load (expand-file-name "modeline.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifiers and Keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package god-mode
  :init
  (defun god-mode-update-cursor ()
    (setq cursor-type
          (if (or god-local-mode buffer-read-only)
              'box
            'bar)))
  
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)

  (defun update-lispy ()
    (if (bound-and-true-p lispy-mode)
        (lispy-mode -1)
      (lispy-mode 1)))
  
  (add-hook 'god-mode-enabled-hook 'update-lispy)
  (add-hook 'god-mode-disabled-hook 'update-lispy)

  :config
  ;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (define-key god-local-mode-map (kbd ".") 'repeat)
  (god-mode-all)
  (add-to-list 'god-exempt-major-modes 'browse-kill-ring-mode)
  (add-to-list 'god-exempt-major-modes 'cider-test-report-mode))


;; (global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x d") 'projectile-dired)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-o") 'other-window)


(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)


(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-c I") 'find-user-init-file)
(global-set-key (kbd "C-c N") 'find-notes-file)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)

;; function key bindings
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "<f9>") 'cycle-theme)
(global-set-key (kbd "<f12>") 'ibuffer)

;; god-mode helpers
(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-2") 'split-window-below)
(global-set-key (kbd "C-x C-3") 'split-window-right)
(global-set-key (kbd "C-x C-0") 'delete-window)


(use-package ace-window
  :defer t
  :init (global-set-key (kbd "M-p") 'ace-window))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define god-local-mode-map "vn" (lambda () (interactive) (scroll-up 10) (next-line 10)))
  (key-chord-define god-local-mode-map "vp" (lambda () (interactive) (scroll-down 10) (previous-line 10)))
  (key-chord-define-global "jv" 'avy-goto-char-2)
  (key-chord-define-global "jw" 'ace-window)
  (key-chord-define-global "jc" 'save-buffer)
  (key-chord-define-global "jf" 'projectile-find-file)
  (key-chord-define-global "jp" 'projectile-switch-project)
  (key-chord-define-global "fb" 'ido-switch-buffer)
  (key-chord-define-global "cv" 'recenter)
  ;; (key-chord-define-global "mk" 'multiple-cursors-hydra/body)
  (key-chord-define-global "fd" 'god-local-mode))


(use-package which-key
  :diminish (which-key-mode . "Ꙍ")
  ;; :diminish (which-key-mode)
  :config
  (which-key-mode)
  (which-key-enable-god-mode-support))


;;;;;;;;;;;;;
; Packages ;;
;;;;;;;;;;;;;

;; indent unless point is at the end of a symbol
;; (use-package smart-tab
;;   :init (bind-key "<tab>" 'hippie-expand read-expression-map))

(use-package ivy
  :init
  (setq ivy-height 20)
  (use-package ivy-hydra)
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package counsel)


(use-package exec-path-from-shell
  :defer 1
  :config
  (when ccann/is-osx
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs (ccann/get-envs "~/.profile"))
    (exec-path-from-shell-copy-env "PYTHONPATH")))

;; (use-package smex
;;   :bind (("M-x" . smex))
;;   :config (smex-initialize))  ; smart meta-x (use IDO in minibuffer)

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
;;(use-package rainbow-mode :defer t)

(use-package company-anaconda
  :defer t
  :diminish anaconda-mode)

(use-package company
  :defer 2
  :init
  (setq company-idle-delay 0.2
        company-minimum-p
        refix-length 3)
  (global-set-key (kbd "<S-tab>") #'company-indent-or-complete-common)
  (setq tab-always-indent t) ;; set this to nil if you want `indent-for-tab-command` instead
  :diminish company-mode
  :config
  (use-package company-quickhelp
    :config (company-quickhelp-mode 1))
  (add-to-list 'company-backends 'company-anaconda)
  (global-company-mode))

(use-package dired-details+
  :defer t)

(use-package dired+
  :defer t)

(save-place-mode 1)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/mule-info nil
        ;; sml/numbers-separator "--"
        sml/show-remote nil
        sml/modified-char " × "
        sml/projectile-replacement-format "[%s] "
        ;; fix graphical artifact in row number
        mode-line-format (cons " " mode-line-format)
        rm-whitelist '(" LY"))
  (sml/setup))


;; (define-fringe-bitmap 'right-truncation
;;   [#b00000000
;;    #b00000000
;;    #b00000000
;;    #b00000000
;;    #b01110000
;;    #b00010000
;;    #b00010000
;;    #b00000000])

;; (define-fringe-bitmap 'left-truncation
;;   [#b00000000
;;    #b00001000
;;    #b00001000
;;    #b00001110
;;    #b00000000
;;    #b00000000
;;    #b00000000
;;    #b00000000])

(define-fringe-bitmap 'right-truncation 
  "\xa9\x02\x04" nil nil 'bottom) 
(define-fringe-bitmap 'left-truncation 
  "\x20\x40\xaa\0\0" nil nil 'bottom) 
(define-fringe-bitmap 'right-continuation 
  "\xa8\0\0" nil nil 'bottom) 
(define-fringe-bitmap 'left-continuation 
  "\x2a\0\0" nil nil 'bottom)

(let ((tr (assoc 'truncation fringe-indicator-alist)) 
      (co (assoc 'continuation fringe-indicator-alist))) 
  (if tr (setcdr tr '(left-truncation right-truncation))) 
  (if co (setcdr co '(left-continuation right-continuation))))

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :init
  ;; (add-hook 'flycheck-mode-hook
  ;;           (lambda () (set-face-attribute 'nlinum nil :underline nil)))
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
    :fringe-face 'flycheck-fringe-info)

  :config
  
  ;; (set-face-attribute 'flycheck-error nil
  ;;                     :foreground (face-attribute 'error :foreground)
  ;;                     :background (face-attribute 'error :background)
  ;;                     :underline 'unspecified)
  ;; (set-face-attribute 'flycheck-warning nil
  ;;                     :foreground (face-attribute 'warning :foreground)
  ;;                     :background (face-attribute 'warning :background)
  ;;                     :underline 'unspecified)
  ;; (set-face-attribute 'flycheck-info nil
  ;;                     :foreground (face-attribute 'info :foreground)
  ;;                     :background (face-attribute 'info :background)
  ;;                     :underline 'unspecified)
  )


(use-package fill-column-indicator
  :init
  (setq fci-rule-column 99
        fci-rule-use-dashes t
        fci-dash-pattern 0.75
        fci-rule-width 1
        ;; fci-rule-color
        ))

(use-package indent-guide :disabled t)

(use-package highlight-symbol
  :init
  ;; autocycle highlighter colors
  (setq hi-lock-auto-select-face t
        highlight-symbol-idle-delay 0.5)
  :bind
  ;; toggle highlighting of symbol at point throughout buffer
  (("C-h s" . highlight-symbol)))
  
(use-package nyan-mode
  :config (nyan-mode -1))

;;;;;;;;;;;;;;;;;;
;; markup modes ;;
;;;;;;;;;;;;;;;;;;


(use-package ob-ipython)

(use-package org
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
     (sh . t)
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


(use-package pov-mode
  :mode ("\\.pov\\'" . pov-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package terraform-mode)

(use-package git-timemachine)

(add-to-list 'auto-mode-alist '("\\.cql\\'" . sql-mode))


;;(use-package auctex
;;  :mode ("\\.tex\\'" . latex-mode)
;;  :commands (latex-mode LaTeX-mode plain-tex-mode)
;;  :config
;;  (if ccann/is-osx
;;      (setq TeX-view-program-selection '((output-pdf "Preview")))
;;    (setq TeX-view-program-selection '((output-pdf "Evince"))))
;;  :init
;;  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
;;  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
;;  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
;;  (add-hook 'LaTeX-mode-hook #'auctex-latexmk-setup)
;;  (setq TeX-auto-save t
;;        TeX-parse-self t
;;        TeX-save-query nil
;;        TeX-PDF-mode t
;;        TeX-view-program-list '(("Preview" "open /Applications/Preview.app %o"
;;                                 "Evince" "evince --page-index=%(outpage) %o")))
;;  (setq-default TeX-master nil))

;;(use-package auctex-latexmk
;;  :defer t
;;  :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)
;;  :commands auctex-latexmk-setup)

;;(use-package reftex
;;  :defer t
;;  :commands turn-on-reftex
;;  :init (setq reftex-plug-into-AUCTeX t))

;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 89)
(show-paren-mode 1)
(global-hl-line-mode 1)

(use-package lispy
  :defer t
  :init
  (setq lispy-compat '(cider edebug))
  (define-advice git-timemachine-mode (:after (&optional arg))
    (if (bound-and-true-p git-timemachine-mode)
        (lispy-mode -1)
      (lispy-mode 1))))

(use-package parinfer :defer t)

(use-package ess :defer t)

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook #'elpy-mode)
  (add-hook 'python-mode-hook #'elpy-enable)
  (add-hook 'python-mode-hook (lambda () (elpy-use-ipython "ipython")))
  (add-hook 'python-mode-hook #'subword-mode)
  ;; (add-hook 'python-mode-hook #'rainbow-delimiters-mode)

  (add-hook 'python-mode-hook #'flycheck-mode)
  (add-hook 'python-mode-hook #'eldoc-mode)

  (setq-default python-indent-guess-indent-offset nil
                python-indent-offset 4)
  (setq python-fill-docstring-style 'pep-257-nn
        python-check-command "flake8"))

(use-package elpy
  :init
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
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  ;; (add-hook 'clojure-mode-hook 'flycheck-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'highlight-symbol-mode)
  ;; (add-hook 'cider-repl-mode-hook (lambda () (hi-lock-mode -1)))
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'fci-mode)
  
  :config
  (set-face-attribute 'clojure-keyword-face nil :weight 'normal)
  (define-clojure-indent
    (db-schema 'defun)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PATCH 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (defapi 'defun)
    (swaggered 'defun)
    (swagger-docs 2))

  (use-package clj-refactor
    :defer t
    :init
    (setq cljr-suppress-middleware-warnings t
          cljr-warn-on-eval nil
          cljr-favor-prefix-notation t
          cljr-project-clean-prompt nil
          cljr-magic-require-namespaces
          '(("io" . "clojure.java.io")
            ("set" . "clojure.set")
            ("str" . "clojure.string")
            ("walk" . "clojure.walk")
            ("zip" . "clojure.zip")
            ("s" . "schema.core"))
          ;; clojure-align-forms-automatically t
          )
    :diminish clj-refactor-mode
    :config (cljr-add-keybindings-with-prefix "C-c C-m"))

  (use-package cider
    :defer t
    :init
    (add-hook 'cider-mode-hook #'clj-refactor-mode)
    (add-hook 'cider-repl-mode-hook #'subword-mode)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)
    
    :config
    (use-package cider-eval-sexp-fu :defer t)
    (setq
     cider-repl-display-help-banner nil
     ;; nrepl-log-messages t                 ; log communication with the nREPL server
     cider-lein-parameters "with-profile +test repl :headless"
     cider-repl-display-in-current-window t
     cider-repl-use-clojure-font-lock t
     cider-save-file-on-load nil
     cider-prompt-for-symbol nil
     cider-stacktrace-fill-column 80
     cider-font-lock-max-length 10000
     cider-repl-use-pretty-printing t
     cider-font-lock-dynamically '(macro core function var)
     nrepl-hide-special-buffers t       ; hide *nrepl-connection* and *nrepl-server*
     cider-overlays-use-font-lock t
     nrepl-prompt-to-kill-server-buffer-on-quit nil)
    (add-hook 'cider-popup-buffer-mode-hook
              (lambda ()
                (when (string= (buffer-name) "*cider-grimoire*")
                  (markdown-mode))))))


(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(diminish 'auto-revert-mode)

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :init
  (setq js-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(use-package projectile
  :pin melpa-stable
  :init
  (setq projectile-enable-caching t)
  :diminish projectile-mode
  :config
  ;; change the shell to sh from bash because I use fish
  (setq shell-file-name "/bin/sh")
  (projectile-global-mode 1))

(use-package eyebrowse
  :config (eyebrowse-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Display Settings ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq frame-resize-pixelwise t)
(column-number-mode 1)
(blink-cursor-mode 1)
(set-fringe-mode '(10 . 5))
(setq visible-bell nil) ; if visible-bell nil, ring-bell-function is alarm
(setq ring-bell-function `(lambda () )) ; empty alarm function. voila.
(setq inhibit-startup-screen t) ; turn off splash screen
(setq ns-use-srgb-colorspace t)
(if ccann/is-osx
    (set-face-attribute 'default nil
                        ;; :weight 'normal
                        ;; :font "Inconsolata-13"
                        :font "Office Code Pro-11"
                        ;; :font "Hack-11"
                        )
  (progn
    (menu-bar-mode 1)
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


(use-package nlinum
  :disabled t
  :init
  ;; prevent nlinum-mode and text-scale-adjust from fucking each other, not a perfect solution
  (add-hook 'nlinum-mode-hook
            (lambda ()
              (set-face-attribute 'linum nil :height 100)))
  (setq nlinum-format "%4d ")

  :config
  (global-nlinum-mode 1))


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

(use-package avy
  :defer t)

;;;;;;;;;
; Misc ;;
;;;;;;;;;
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(global-auto-revert-mode t) ; automatically reload changed buffers
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil) ; disallow tab insertion

;; (use-package smooth-scrolling
;;   :config
;;   (smooth-scrolling-mode 1))

(setq mouse-wheel-scroll-amount '(3)) ;; n lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse

(setq-default bidi-display-reordering nil)


(use-package sublimity
  :disabled t
  :init
  (require 'sublimity-scroll)
  (setq sublimity-scroll-weight 4
        sublimity-scroll-drift-length 6)
  :config (sublimity-mode 1))



; configure clipoard
(setq save-interprogram-paste-before-kill t
      ; Mouse-2 inserts text at point, not click location
      mouse-yank-at-point t) 

(delete-selection-mode 1)

(use-package visual-regexp
  :defer t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))
  
(setq scroll-error-top-bottom t)

(use-package browse-kill-ring
  :init
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item 'pulse
        browse-kill-ring-separator ""
        browse-kill-ring-show-preview nil)
  :bind ("M-y" . browse-kill-ring))

(use-package google-this
  :defer t
  :bind ("C-x g" . google-this))


(use-package neotree
  :ensure t
  :bind (("<f2>" . neotree-toggle))
  :init
  ;; (add-hook 'projectile-after-switch-project-hook #'neotree-projectile-action)

  (setq neo-theme 'icons
        neo-smart-open t))

;;;;;;;;;;;
; themes ;;
;;;;;;;;;;;
(use-package all-the-icons)
;; run M-x all-the-icons-install-fonts once


;; TODO add color theme support for matching parens like doom-themes do
(use-package flatui-theme)
(use-package doom-themes)
(use-package gruvbox-theme)
(use-package zenburn-theme)
(use-package badger-theme)
(use-package gotham-theme)
(use-package darktooth-theme)
(use-package material-theme)
(use-package metalheart-theme)
(use-package apropospriate-theme)
(use-package ample-theme)
(use-package challenger-deep-theme)
(use-package spacemacs-theme)
(use-package kaolin-theme)



(defvar curr-theme nil)
(defvar my-themes '(
                    ;; ample-light
                    kaolin
                    doom-one
                    gruvbox
                    ;; darktooth
                    ;; challenger-deep
                    spacemacs-dark
                    apropospriate-dark
                    flatui
                    apropospriate-light))
(cycle-my-theme)


;; init.el ends here

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
