;;; settings.el --- Colors, fonts, look and feel, and basic extensions

;;; Commentary: 
;;; I pulled a lot of this from better-defaults.el

;;; Code:

;; magic comment
;;;###autoload
(progn

  (setq ns-use-srgb-colorspace t) 

  (setq exec-path (append exec-path '("/usr/local/bin"))) ;; need this?
  
  ;; fonts and menu bar
  (if (eq system-type 'darwin)
      (progn
        (set-face-attribute 'default nil :font "Droid Sans Mono-11")
        (menu-bar-mode 1))
    (progn
      (set-face-attribute 'default nil :font "Ubuntu Mono-10")
      (menu-bar-mode 0)))

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

  (setq-default fill-column 90)
  (column-number-mode -1)
  (blink-cursor-mode 1)
  (global-hl-line-mode 1)

  ;; if visible-bell nil, ring-bell-function is alarm
  (setq visible-bell nil)
  ;; empty alarm function. voila.
  (setq ring-bell-function `(lambda () ))

  (show-paren-mode 1)

  ;; prevent extraneous tabs
  (setq-default indent-tabs-mode nil)

  (setq x-select-enable-clipboard t  ;; use the X clipboard
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t  ;; search for noninteractive functions, too (C-h a)
        mouse-yank-at-point t ;; Mouse-2 inserts text at point, not click location
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

(provide 'settings)
;;; settings.el ends here

