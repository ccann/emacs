;;; config.el --- Colors, fonts, look and feel, and basic extensions

;;; Commentary: 
;;; I pulled a lot of this from better-defaults.el

;;; Code:

;; configure frame
(set-fringe-mode '(1 . 0)) ;; turn off right fringe
(setq visible-bell nil) ;; if visible-bell nil, ring-bell-function is alarm
(setq ring-bell-function `(lambda () )) ;; empty alarm function. voila.
(setq inhibit-startup-screen t) ;; turn off splash screen
(setq ns-use-srgb-colorspace t)
(if (eq system-type 'darwin)
    (progn
      (menu-bar-mode 1)
      (set-face-attribute 'default nil :font "Source Code Pro-11"))
  (progn
    (menu-bar-mode 0)
    (set-face-attribute 'default nil :font "Source Code Pro-10")))
(when (fboundp 'scroll-bar-mode) ;; turn off scroll bar and tool bar
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; (setq initial-frame-alist ;; I dont think this does anything ... at least on OSX
;;       '((menu-bar-lines . 0)
;;         (tool-bar-lines . 0)))
(column-number-mode 1) ;; show column number in modeline
(blink-cursor-mode 1) 
(setq default-major-mode 'text-mode)

;; Scratch buffer 
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "#+TITLE: Scratch\n\n")

;; killing emacs
(setq confirm-kill-emacs 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p) ;; change to 'y' or 'n'

;; misc. configurations
(setq apropos-do-all t) ;; search for non-interactive functions, too (C-h a)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(global-auto-revert-mode t) ;; automatically reload changed buffers
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq exec-path (append exec-path '("~/.local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil) ;; prevent extraneous tabs

;; configure smooth scrolling
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; configure clipoard
(setq x-select-enable-clipboard t  ;; use the X clipboard
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t ;; Mouse-2 inserts text at point, not click location
)

;; configure ido
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(smex-initialize) ;; smart meta-x (use IDO in minibuffer)

;; programming related settings
(setq-default fill-column 79)
(show-paren-mode 1)
(global-hl-line-mode 1)

;; configure ibuffer
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

;; configure dired
(require 'dired-details)
(setq ls-lisp-use-insert-directory-program t)
(setq dired-use-ls-dired nil) 
(dired-details-install)

;; configure smart-mode-line
(sml/setup)
(setq sml/mule-info nil)
(setq sml/numbers-separator ":")
(setq sml/show-remote nil)
(setq sml/modified-char " x ")

;; configure uniqify
(require 'uniquify) ;; use prepended directory for unique files
(setq uniquify-buffer-name-style 'forward) 

;; configure saveplace
(require 'saveplace) ;; save place in the buffer for next visit
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; configure magit
(when (eq system-type 'darwin)
  (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))

;; configure indent-guide
;; (require 'indent-guide) ;; show vertical guide to indentation level
;; (setq indent-guide-char "|")
;; (indent-guide-global-mode)

;; configure fci
(require 'fill-column-indicator) ;; show vertical line at fill-column

;; configure zone
(require 'zone)
(zone-when-idle 300) ;; zone out after 300 seconds

;; when non-nil cycle through faces in hi-lock-faces-defaults instead of prompting
(setq hi-lock-auto-select-face t)

;; configure popwin
(require 'popwin)
(popwin-mode 1)

;; configure highlight-symbol
(require 'highlight-symbol)
(add-hook 'prog-mode-hook (lambda () (progn
                                       (highlight-symbol-mode)
                                       (highlight-symbol-nav-mode 1))))
(add-hook 'org-mode-hook (lambda () (progn
                                      (highlight-symbol-mode)
                                      (highlight-symbol-nav-mode 1))))
(setq highlight-symbol-on-navigation-p t)

;; configure projectile
(projectile-global-mode)


;;; config.el ends here
