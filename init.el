;;; init.el --- My init file for emacs

;;; Commentary:
;;; LaTeX installation on OSX 10.11.1
;;;  $ brew up
;;;  $ brew cask install mactex
;;; # add /usr/texbin to PATH (in /usr/local/texlive)

;; author: ccann

;;; Code:

;; (setq-default warning-minimum-level :emergency)

;; BUGFIX: remove this after 26.3+
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'init-funcs)
(require 'init-package)
(require 'init-basic)
(require 'init-java)

(use-package list-utils)
(use-package hydra)
;; (use-package popwin :config (popwin-mode 1))
(diminish 'auto-revert-mode)

;; Elisp
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'emacs-lisp-mode-hook #'paren-face-mode)

(setq gc-cons-threshold 100000000)
(use-package gnu-elpa-keyring-update)
(setq package-check-signature nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifiers and Keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cursor-type 'bar)

(use-package god-mode
  :init
  (defun god-mode-update-cursor ()
    (setq cursor-type
          (if (or god-local-mode buffer-read-only)
              'box
            'bar)))

  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)

  :config
  ;; (define-key god-local-mode-map (kbd "i") 'god-local-mode)
  (define-key god-local-mode-map (kbd ".") 'repeat)
  (god-mode-all)
  (add-to-list 'god-exempt-major-modes 'browse-kill-ring-mode)
  (add-to-list 'god-exempt-major-modes 'cider-test-report-mode))

;; (bind-key* "C-x d" 'dired-jump)
(bind-key* "C-x d" 'projectile-dired)

;; function key bindings
(bind-key* "<f9>" 'ccann/cycle-theme)

;; god-mode helpers
(bind-key* "C-x C-1 " 'delete-other-windows)
(bind-key* "C-x C-2" 'split-window-below)
(bind-key* "C-x C-3" 'split-window-right)
(bind-key* "<f10>" 'magit-status)
(bind-key* "C-x C-0" 'delete-window)

(use-package ace-window
  :init (global-set-key (kbd "M-p") 'ace-window))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define god-local-mode-map "vn" (lambda () (interactive) (scroll-up 10) (next-line 10)))
  (key-chord-define god-local-mode-map "vp" (lambda () (interactive) (scroll-down 10) (previous-line 10)))
  (key-chord-define-global "jv" 'avy-goto-char-2)
  (key-chord-define-global "jw" 'ace-window)
  (key-chord-define-global "jc" 'save-buffer)
  ;; (key-chord-define-global "jf" 'projectile-find-file)
  (key-chord-define-global "jf" 'counsel-projectile)
  (key-chord-define-global "jp" 'projectile-switch-project)
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

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content t)
  (setq dashboard-items '((projects . 8) (recents  . 10)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  :config
  (dashboard-setup-startup-hook))

(use-package counsel
  ;; Counsel - a collection of Ivy-enhanced versions of common Emacs commands.
  ;; -- Counsel will install Ivy and Swiper as dependencies --
  ;; Ivy - A generic completion frontend.
  ;; Swiper - isearch with an overview, and more.
  :init
  (setq ivy-height 25)
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point))
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (ivy-switch-buffer . ivy--regex-ignore-order)
          (counsel-projectile-find-file . ivy--regex-ignore-order)
          (counsel-projectile . ivy--regex-ignore-order)
          (counsel-M-x . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (bind-key* "C-s" 'swiper)
  (bind-key* "M-y" 'counsel-yank-pop)
  (bind-key* "C-x C-f" 'counsel-find-file)
  (bind-key* "M-x" 'counsel-M-x)
  (bind-key* "<f6>" 'ivy-resume)
  (key-chord-define-global "fb" 'ivy-switch-buffer)

  ;; if this is t it will yank the most recent kill, instead of the second most
  ;; (setq counsel-yank-pop-preselect-last t)

  (ivy-mode 1))

(use-package prescient
  :defer t)

(use-package ivy-prescient
  :defer t
  :after (counsel prescient)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package ivy-posframe
  ;; pop a child frame to show ivy completion
  :after (counsel)
  :config
  (setq ivy-posframe-display-functions-alist
        ;; use minibuffer default for swiper because frame will occlude buffer
        '((swiper          . nil)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 5)))
  (ivy-posframe-mode 1))

;; Matching engine
(use-package flx)

(use-package ivy-hydra
  ;; Additional keybindings for Ivy.
  :defer t)

(use-package all-the-icons-ivy-rich
  ;; Better experience with icons
  ;; Enable it before`ivy-rich-mode' for better performance
  :if (and (display-graphic-p)
           (require 'all-the-icons nil t))
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  ;; More friendly display transformer for Ivy
  :hook (;; Must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(use-package ido-vertical-mode
  ;; Makes ido-mode display vertically
  :config (ido-vertical-mode 1))

(use-package magit
  :diminish magit-auto-revert-mode
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-display-buffer-function  #'magit-display-buffer-fullframe-status-v1)
  :bind (("<f10>" . magit-status))
  :config
  (when sys/mac-cocoa-p
    (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")))

(use-package rainbow-delimiters)

(use-package company-anaconda
  :defer t
  :diminish anaconda-mode)

(use-package company
  :defer 1
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  ;; set this to nil if you want `indent-for-tab-command` instead
  ;; (setq tab-always-indent 'complete)
  :diminish company-mode
  :config
  (add-to-list 'company-backends 'company-anaconda)
  (setq company-global-modes
        '(emacs-lisp-mode
          clojure-mode
          clojurescript-mode
          clojurec-mode
          python-mode
          web-mode
          java-mode))
  (global-company-mode))

(use-package smart-mode-line
  :disabled t
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
    :fringe-face 'flycheck-fringe-info)

  :config
  (setq flycheck-display-errors-delay 0.25)
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))


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
;; (use-package flycheck-popup-tip
;;   :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
;;   :config
;;   (setq flycheck-popup-tip-error-prefix "✕ "))

(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ ")
  (flycheck-posframe-configure-pretty-defaults)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package fill-column-indicator
  :init
  (setq fci-rule-column 99
        fci-rule-use-dashes t
        fci-dash-pattern 0.75
        fci-rule-width 1))

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


;; (use-package ob-ipython
;;   :after (org))

;; (use-package htmlize
;;   :after (org))

;; cool bullet: ⁖
;; (use-package org
;;   :pin gnu
;;   :mode ("\\.org\\'" . org-mode)
;;   :init
;;   ;; (setq org-directory "~/Dropbox/org")
;;   (add-hook 'org-mode-hook #'visual-line-mode)
;;   (add-hook 'org-mode-hook #'org-indent-mode)
;;   (add-hook 'org-mode-hook #'auto-fill-mode)

;;   :bind (("<f8>" . org-capture))
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((emacs-lisp . t)
;;      (clojure . t)
;;      (sh . t)
;;      (python . t)))

;;   ;; the following jekyll-boostrap integration depends on ~/dev/ccann.github.io
;;   ;; and ~/blog existing, see below.
;;   (setq org-hide-emphasis-markers nil)

;;   (setq org-publish-project-alist
;;         '(("org-ccann"
;;            :base-directory "~/blog" ;; Path to your org files.
;;            :base-extension "org"
;;            :publishing-directory "~/dev/ccann.github.io/_posts" ;; Path to your Jekyll project.
;;            :recursive t
;;            :publishing-function org-html-publish-to-html
;;            :headline-levels 4
;;            :html-extension "html"
;;            :body-only t)
;;           ("org-static-ccann"
;;            :base-directory "~/blog/images"
;;            :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
;;            :publishing-directory "~/dev/ccann.github.io/assets"
;;            :recursive t
;;            :publishing-function org-publish-attachment)
;;           ("blog" :components ("org-ccann" "org-static-ccann"))))

;;   (setq org-default-notes-file (concat org-directory "/notes.org")
;;         org-src-fontify-natively t      ; fontify source blocks
;;         org-html-doctype "html5"
;;         org-html-html5-fancy t
;;         org-html-postamble nil
;;         org-hide-leading-stars t
;;         org-tags-column 85
;;         org-latex-to-pdf-process (list "latexmk -f -pdf")
;;         org-fontify-done-headline nil)

;;   (setq org-capture-templates
;;         '(("l" "Logbook" item
;;            (file+olp+datetree "~/org/logbook.org")
;;            "")
;;           ("t" "TODO" entry
;;            (file+headline "~/org/todo.org" "Tasks")
;;            "* TODO %?\n  %i\n  %a")))

;;   ;; Make Org-mode use evince in linux to open PDFs
;;   (if (not sys/mac-cocoa-p)
;;       (add-hook 'org-mode-hook
;;                 (lambda ()
;;                   (delete '("\\.pdf\\'" . default) org-file-apps)
;;                   (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))))


(use-package pov-mode
  :mode ("\\.pov\\'" . pov-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-open-command "marked")
  (setq markdown-command "marked")
  (setq markdown-css-dir "~/.emacs.d/markdown-css/")
  (setq markdown-css-theme "clearness"))

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package terraform-mode
  :init
  (add-hook 'terraform-mode-hook #'highlight-symbol-mode)
  :bind (("C-c j" . ccann/insert-terraform-interpolation)
         ("C-c k" . ccann/insert-terraform-interpolation-nested)
         ("C-c a" . ccann/align-terraform-region)))

(use-package git-timemachine)

(add-to-list 'auto-mode-alist '("\\.cql\\'" . sql-mode))

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (if ccann/mac-cocoa-p
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
  :init (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :commands auctex-latexmk-setup)

(use-package reftex
  :commands turn-on-reftex
  :init (setq reftex-plug-into-AUCTeX t))

;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Modes ;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq-default fill-column 83)
(show-paren-mode 1)
(global-hl-line-mode 1)

(use-package omnisharp :defer t)

(use-package lispy
  :defer t
  :init
  (setq lispy-compat '(edebug cider))
  (define-advice git-timemachine-mode (:after (&optional arg))
    (if (bound-and-true-p git-timemachine-mode)
        (lispy-mode -1)
      (lispy-mode 1)))

  (defun update-lispy ()
    (if (or (eq major-mode 'emacs-lisp-mode)
            (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode)
            (eq major-mode 'clojurec-mode))
        (if (bound-and-true-p lispy-mode)
            (lispy-mode -1)
          (lispy-mode 1))
      (lispy-mode -1)))

  (add-hook 'god-mode-enabled-hook #'update-lispy)
  (add-hook 'god-mode-disabled-hook #'update-lispy))

;; (setq auth-sources
;;       (quote (macos-keychain-internet macos-keychain-generic)))


(use-package robe)

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-hook 'ruby-mode-hook #'highlight-symbol-mode))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook #'elpy-mode)
  (add-hook 'python-mode-hook #'highlight-symbol-mode)
  (add-hook 'python-mode-hook #'elpy-enable)
  (add-hook 'python-mode-hook (lambda () (elpy-use-ipython "ipython")))
  (add-hook 'python-mode-hook #'subword-mode)
  ;; (add-hook 'python-mode-hook #'rainbow-delimiters-mode)

  (add-hook 'python-mode-hook #'eldoc-mode)

  (setq-default python-indent-guess-indent-offset nil
                python-indent-offset 4)
  (setq python-fill-docstring-style 'pep-257-nn
        python-check-command "flake8"))

(use-package elpy
  :defer t
  :init
  (setq elpy-rpc-backend "jedi")
  (setq elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-sane-defaults
                       elpy-module-pyvenv))
  (when sys/mac-cocoa-p
    (setq elpy-rpc-python-command "/usr/local/bin/python3")))

(use-package jedi
  :init (setq jedi:complete-on-dot t)
  :bind (("C-c d" . jedi:show-doc)))

(use-package lua-mode)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode)

(use-package eldoc
  :diminish eldoc-mode)

(use-package paren-face)

(add-hook 'nxml-mode-hook #'(lambda () (setq require-final-newline nil)))

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(defun +clojure--cider-dump-nrepl-server-log-h ()
  "Copy contents of *nrepl-server* to beginning of *cider-repl*."
  (when (buffer-live-p nrepl-server-buffer)
    (save-excursion
      (goto-char (point-min))
      (insert
       (with-current-buffer nrepl-server-buffer
         (buffer-string))))))

(use-package cider
  ;; CIDER extends Emacs with support for interactive programming in Clojure. The
  ;; features are centered around cider-mode, a minor-mode that complements clojure-mode.
  :defer t
  :pin melpa-stable
  :init
  (add-hook 'clojure-mode-hook #'cider-mode)
  (setq cider-repl-display-help-banner nil
        cider-repl-display-in-current-window t
        cider-save-file-on-load nil
        cider-prompt-for-symbol nil
        ;; cider-redirect-server-output-to-repl nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file "~/.emacs.d/cider-repl-history"
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-print-length 100
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history nil
        cider-repl-use-pretty-printing t
        cider-stacktrace-default-filters '(tooling dup)
        cider-stacktrace-fill-column 90
        cider-auto-select-error-buffer t
        cider-font-lock-max-length 10000
        cider-font-lock-dynamically '(macro core deprecated function var)
        ;; hide *nrepl-connection* and *nrepl-server*
        nrepl-hide-special-buffers nil
        cider-overlays-use-font-lock t
        cider-repl-pop-to-buffer-on-connect 'display-only
        nrepl-prompt-to-kill-server-buffer-on-quit nil)
  ;; (setq cider-default-cljs-repl 'shadow)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'lispy-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook (lambda () (hi-lock-mode -1)))

  (add-hook 'cider-connected-hook #'+clojure--cider-dump-nrepl-server-log-h)

  ;; (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  ;; useful for cider-grimoire command which produces a markdown buffer
  (add-hook 'cider-popup-buffer-mode-hook
            (lambda ()
              (when (string= (buffer-name) "*cider-grimoire*")
                (markdown-mode))))

  :bind
  ;; SPC m e b	eval buffer
  ;; SPC m e e	eval last sexp
  ;; SPC m e f	eval function at point
  ;; SPC m e r	eval region
  ;; SPC m e m	cider macroexpand 1
  ;; SPC m e M	cider macroexpand all
  ;; SPC m e p	print last sexp (clojure interaction mode only)
  ;; SPC m e w	eval last sexp and replace with result
  (("C-c e f" . cider-eval-defun-at-point)
   ("C-c e r" . cider-eval-region)
   ("C-c e e" . cider-eval-last-sexp)
   ("C-c e m" . cider-macroexpand-1)
   ("C-c e M" . cider-macroexpand-all)
   ("C-c e p" . cider-pprint-eval-last-sexp)
   ("C-c e w" . cider-eval-last-sexp-and-replace)
   ("C-c e b" . cider-load-buffer)

   ;; SPC m t a	run all tests in namespace
   ;; SPC m t r	re-run test failures for namespace
   ;; SPC m t t	run test at point
   ("C-c t t" . cider-test-run-test)
   ("C-c t n" . cider-test-run-ns-tests)
   ("C-c t p" . cider-test-run-project-tests)
   ("C-c t r" . cider-test-show-report)
   ("C-c t f" . cider-test-rerun-failed-tests)
   ("C-c e" . jet/json->edn)))

;; (load (expand-file-name "eftest-runner.el" user-emacs-directory))
;; (require 'eftest-runner)
;; (define-key clojure-mode-map (kbd "C-c k t") 'eftest-runner-run-test-at-point)
;; (define-key clojure-mode-map (kbd "C-c k r") 'eftest-runner-run-tests)
;; (define-key clojure-mode-map (kbd "C-c k a") 'eftest-runner-run-all-tests)
;; (define-key clojure-mode-map (kbd "C-c k w") 'eftest-runner-show-warnings)
;; (define-key clojure-mode-map (kbd "C-c k h") 'eftest-runner-hide-windows)

(use-package cider-eval-sexp-fu
  ;; Provides tiny improvements to CIDER expression evaluation - e.g. the expression
  ;; you've just evaluated would briefly flash.
  :after (cider))

(use-package clj-refactor
  ;; Provides refactoring support for Clojure projects. Prefer the `melpa` version as
  ;; there hasn't been a stable release since 2018.
  :defer t
  :pin melpa
  :init
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  ;; don't use clj-refactor's outdated refactor-nrepl dependency
  ;; (setq cljr-inject-dependencies-at-jack-in nil)
  (setq cljr-favor-prefix-notation nil
        cljr-project-clean-prompt nil
        cljr-auto-clean-ns t
        cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("set" . "clojure.set")
          ("str" . "clojure.string")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("s" . "clojure.spec")
          ("log" . "taoensso.timbre")
          ("casex" . "camel-snake-kebab.extras")
          ("case" . "camel-snake-kebab.core")
          ("mount" . "mount.core")
          ("dfs" . "net.danielcompton.defn-spec-alpha")
          ("ig" . "integrant.core")))
  :diminish clj-refactor-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; inject the refactor-nrepl middleware manually
  ;; (add-to-list 'cider-jack-in-nrepl-middlewares "refactor-nrepl.middleware/wrap-refactor")
  )

(use-package clojure-mode
  ;; Provides font-lock, indentation, navigation and refactoring for Clojure(Script).
  :defer t
  :pin melpa
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :diminish (subword-mode)
  :init (setq clojure-docstring-fill-column 80)
  :config
  (setq projectile-project-root-files
        (append projectile-project-root-files
                '("project.clj"
                  "built.boot"
                  "deps.edn")))
  ;; (set-face-attribute 'clojure-keyword-face nil :weight 'normal)
  (add-hook 'clojure-mode-hook #'paren-face-mode)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'highlight-symbol-mode)
  (define-clojure-indent
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
    (swagger-docs 2)))

(use-package flycheck-clj-kondo
  ;; Integrates clj-kondo with Emacs via flycheck.
  :after (flycheck clojure-mode))


(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :init
  (setq css-indent-offset 4)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))


;; sudo npm install -g tern
(use-package js2-mode
  :mode ("\\.js?\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook #'highlight-symbol-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (add-hook 'js2-mode-hook
            (lambda ()
              (tern-mode)
              (company-mode)))
  :config
  (setq js2-basic-offset 2))

(use-package js2-refactor
  :after (js2-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :after (js2-mode)
  :config
  (define-key js-mode-map (kbd "M-.") nil))

;; This has disappeared from MELPA
;; (use-package company-tern
;;   :after (js2-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-tern)
;;   ;; Disable completion keybindings, as we use xref-js2 instead
;;   (define-key tern-mode-keymap (kbd "M-.") nil)
;;   (define-key tern-mode-keymap (kbd "M-,") nil))

(use-package handlebars-mode)

(use-package zoom
  :disabled t
  ;; alternative to golden-ratio
  ;; https://github.com/cyrus-and/zoom#faq
  :init
  (defun size-callback ()
    ;; zoomed buffer  will take 120 columns and 75% of the frame height
    ;; if the frame width is larger than 1280 pixels
    (cond ((> (frame-pixel-width) 1280) '(120 . 0.75))
          ;; otherwise 50-50
          (t '(0.5 . 0.5))))
  :config
  (setq zoom-size 'size-callback)
  (zoom-mode 1))

(use-package counsel-projectile
  ;; Ivy UI for Projectile
  ;; https://github.com/ericdanan/counsel-projectile
  )

(use-package projectile
  :pin melpa-stable
  :init
  (setq projectile-ignored-projects '("~/dev/thanks"
                                      "~/flipboard/repos/clj-services"))
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  :diminish projectile-mode
  :config
  ;; change the shell to sh from bash because I use fish
  (setq shell-file-name "/bin/sh")
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (counsel-projectile-mode))

(use-package ripgrep)

(use-package projectile-ripgrep
  :after (ripgrep projectile))

(use-package eyebrowse
  :config (eyebrowse-mode))

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Display Settings ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq frame-title-format "%b")
(setq-default tab-width 2)
(setq frame-resize-pixelwise t)

(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(set-fringe-mode '(10 . 5))


(setq inhibit-startup-screen t) ; turn off splash screen
(if sys/mac-cocoa-p
    (progn
      (set-face-attribute 'default nil
                          ;; :weight 'normal
                          ;; :font "Monaco-13"
                          :font "Office Code Pro-16")
      (menu-bar-mode 1))
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

(use-package olivetti
  :defer t
  :init
  (setq-default olivetti-body-width 90))

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package highlight-numbers :defer t)


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

(use-package avy :defer t)

;;;;;;;;;
; Misc ;;
;;;;;;;;;
(setq confirm-kill-emacs 'y-or-n-p)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(global-auto-revert-mode t) ; automatically reload changed buffers
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil) ; disallow tab insertion

(setq-default bidi-display-reordering nil)


; configure clipoard
(setq save-interprogram-paste-before-kill t
      ; Mouse-2 inserts text at point, not click location
      mouse-yank-at-point t)

(delete-selection-mode 1)

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

(use-package visual-regexp-steroids
  :after (visual-regexp))

(setq scroll-error-top-bottom t)

(use-package google-this
  :bind ("C-x g" . google-this))

(use-package neotree
  :bind (("<f2>" . neotree-toggle))
  :init
  (bind-key* "<f2>" 'neotree-toggle)
  (setq neo-theme 'icons
        neo-smart-open t
        neo-window-width 45
        ;; projectile-switch-project-action 'neotree-projectile-action
        ))

;; (use-package treemacs
;;   :bind (("<f2>" . treemacs))
;;   :defer t
;;   :ensure t
;;   :config (setq treemacs-project-follow-cleanup t))

;; (use-package treemacs-projectile
;;   :after (treemacs projectile))

;; (use-package treemacs-magit
;;   :after (treemacs magit))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)



(use-package direnv
 :config
 (direnv-mode))


;;;;;;;;;;;;
;; themes ;;
;;;;;;;;;;;;
(use-package all-the-icons)
;; run M-x all-the-icons-install-fonts once

(use-package flatui-theme :defer t)
;; (use-package gruvbox-theme :defer t)
;; (use-package zenburn-theme :defer t)
;; (use-package badger-theme :defer t)
;; (use-package gotham-theme :defer t)
;; (use-package darktooth-theme :defer t)
;; (use-package material-theme :defer t)
;; (use-package metalheart-theme :defer t)
;; (use-package apropospriate-theme :defer t)
;; (use-package ample-theme :defer t)
;; (use-package challenger-deep-theme :defer t)
;; (use-package spacemacs-theme :defer t)
;; (use-package panda-theme :defer t)
(use-package kaolin-themes)
(use-package chocolate-theme)
(use-package doom-themes
  :config
  (doom-themes-treemacs-config)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))


(defvar curr-theme nil)
(defvar my-themes
  '(
    ;; dark themes
    doom-wilmersdorf
    doom-spacegrey
    doom-laserwave
    doom-city-lights
    kaolin-temple
    kaolin-dark
    ;; kaolin-mono-dark
    kaolin-aurora
    kaolin-eclipse
    kaolin-ocean
    kaolin-galaxy
    kaolin-valley-dark
    doom-one
    chocolate

    ;; doom-vibrant
    ;; darktooth
    ;; Light Themes
    flatui
    kaolin-light
    kaolin-valley-light
    kaolin-breeze))

;; (custom-theme-set-faces 'darktooth
;;                         '(vertical-border ((t (:foreground "#504945")))))

(ccann/cycle-theme)

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :config
  ;; see settings for persp and lsp
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        ;; maximum displayed length of the branch name of version control.
        doom-modeline-vcs-max-length 12
        ;; this could cause performance issues
        ;; auto-revert-check-vc-info t
        doom-modeline-minor-modes nil))

;; (load (expand-file-name "modeline.el" user-emacs-directory))

;; init.el ends here
