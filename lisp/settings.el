;;; settings.el --- Colors, fonts, look and feel, and basic extensions

;;; Commentary: 
;;; I pulled a lot of this from better-defaults.el

;;; Code:

;; magic comment
;;;###autoload
(progn

  
  ;; Scratch buffer 
  (setq initial-major-mode 'org-mode)
  (setq initial-scratch-message "#+TITLE: Scratch\n\n")
  ;;(setq initial-scratch-message "#+TITLE: Note\n#+options: toc:nil num:nil\n\n")
  ;; automatically reload changed buffers
  (global-auto-revert-mode t)

  (setq ns-use-srgb-colorspace t)
  
  (setq exec-path (append exec-path '("/usr/local/bin"))) ;; need this?
  
  ;; fonts and menu bar
  (if (eq system-type 'darwin)
      (menu-bar-mode 1)
    (menu-bar-mode 0))

  (if (eq system-type 'darwin)
      (set-face-attribute 'default nil :font "Source Code Pro-11")
    (set-face-attribute 'default nil :font "Source Code Pro-10"))

  ;; turn off scroll bar and tool bar
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; turn off splash screen
  (setq inhibit-startup-screen t)

  ;; I dont think this does anything ... at least on OSX
  ;; (setq initial-frame-alist
  ;;       '((menu-bar-lines . 0)
  ;;         (tool-bar-lines . 0)))

  ;; turn off right fringe
  (set-fringe-mode '(1 . 0)) 

  (setq-default fill-column 79)
  (column-number-mode 1)
  (blink-cursor-mode 1)
  (global-hl-line-mode 1)

  (setq create-lockfiles nil)
  
  ;; backspace no longer does reverse-search
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

  
  ;; if visible-bell nil, ring-bell-function is alarm
  (setq visible-bell nil)
  ;; empty alarm function. voila.
  (setq ring-bell-function `(lambda () ))

  (show-paren-mode 1)
  (setq-default auto-fill-function 'do-auto-fill)
  (setq default-major-mode 'text-mode)
  (setq scroll-step 1)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  ;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

  
  
  ;; prevent extraneous tabs
  (setq-default indent-tabs-mode nil)

  (setq x-select-enable-clipboard t  ;; use the X clipboard
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t  ;; search for noninteractive functions, too (C-h a)
        mouse-yank-at-point t ;; Mouse-2 inserts text at point, not click location
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups"))))

  (require 'zone)
  (zone-when-idle 300)

  (provide 'settings))
;;; settings.el ends here

