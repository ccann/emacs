;; ;;; modeline.el --- Modeline implementation

;; ;;; Commentary:
;; ;;; adapted from https://github.com/MaxSt/challenger-deep/issues/1

;; ;;; Code:

;; (use-package all-the-icons)

;; (defun custom-modeline-github-vc ()
;;   (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
;;     (concat
;;      (propertize (format "  %s" (all-the-icons-octicon "git-branch"))
;;                  'face `(:height 1.2 :family ,(all-the-icons-octicon-family)
;;                                  :foreground ,(face-attribute 'font-lock-type-face :foreground))
;;                  'display '(raise -0.1))
;;      (propertize (format " %s" branch) 'face
;;                  `(:height 1.0 :foreground ,(face-attribute 'font-lock-type-face :foreground))))))

;; (defun projectile-root ()
;;   "Show the current projectile root."
;;   (if (and (fboundp 'projectile-project-p)
;;            (stringp (projectile-project-p))
;;            (not (string= (projectile-project-name) (buffer-name))))
;;       (format "[%s]" (projectile-project-name))
;;     ""))

;; (use-package powerline
;;   :config
  
;;   (defun custom-modeline-icon-vc ()
;;     (when vc-mode
;;       (cond
;;        ((string-match "Git[:-]" vc-mode) (custom-modeline-github-vc))
;;        (t (format "%s" vc-mode)))))


;;   (defun make-rect (color height width)
;;     "Create an XPM bitmap."
;;     (when window-system
;;       (propertize
;;        " " 'display
;;        (let ((data nil)
;;              (i 0))
;;          (setq data (make-list height (make-list width 1)))
;;          (pl/make-xpm "percent" color color (reverse data))))))


;;   (defun powerline-mode-icon ()
;;     (let ((icon (all-the-icons-icon-for-buffer)))
;;       (unless (symbolp icon) ;; This implies it's the major mode
;;         (format " %s"
;;                 (propertize icon
;;                             'help-echo (format "Major-mode: `%s`" major-mode)
;;                             'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))


;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (modified (buffer-modified-p))
;;                           (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                           (face3 (if active 'font-lock-keyword-face 'powerline-inactive1))
;;                           (spacer (powerline-raw " " face1))
;;                           (bar-color (cond ((and active modified) (face-foreground 'error))
;;                                            (active (face-background 'cursor))
;;                                            (t (face-background 'tooltip))))
;;                           (lhs (list
;;                                 (make-rect bar-color 26 3)
;;                                 (when modified
;;                                   (concat
;;                                    " "
;;                                    (all-the-icons-faicon "floppy-o"
;;                                                          :face (when active 'error)
;;                                                          :v-adjust -0.01)))
;;                                 " "
;;                                 (powerline-raw (projectile-root) face3)
;;                                 (powerline-buffer-id)
;;                                 " "
;;                                 (custom-modeline-icon-vc)))
;;                           (center (list
;;                                    " "
;;                                    (powerline-mode-icon)
;;                                    " "
;;                                    (powerline-major-mode)
;;                                    " "))
;;                           (rhs (list
;;                                 (powerline-raw "%l:%c" 'mode-line)
;;                                 " | "
;;                                 (format "%s" (eyebrowse--get 'current-slot))
;;                                 " | "
;;                                 (powerline-raw "%6p" 'mode-line 'r)
;;                                 (powerline-hud 'highlight 'region 1)
;;                                 " ")))
;;                      (concat
;;                       (powerline-render lhs)
;;                       (powerline-fill-center face1 (/ (powerline-width center) 2.0))
;;                       (powerline-render center)
;;                       (powerline-fill face1 (powerline-width rhs))
;;                       (powerline-render rhs)))))))

;; ;;; modeline.el ends here
(use-package powerline
  :ensure t
  :config

  (defun make-rect (color height width)
    "Create an XPM bitmap."
    (when window-system
      (propertize
       " " 'display
       (let ((data nil)
             (i 0))
         (setq data (make-list height (make-list width 1)))
         (pl/make-xpm "percent" color color (reverse data))))))


  (defun powerline-mode-icon ()
    (let ((icon (all-the-icons-icon-for-buffer)))
      (unless (symbolp icon) ;; This implies it's the major mode
        (format " %s"
                (propertize icon
                            'help-echo (format "Major-mode: `%s`" major-mode)
                            'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))


  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (modified (buffer-modified-p))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 (if active 'mode-line-active 'mode-line-inactive))
                          
                          (bar-color (cond ((and active modified) (face-foreground 'error))
                                           (active (face-background 'cursor))
                                           (t (face-background 'tooltip))))
                          (lhs (list
                                (make-rect bar-color 30 3)
                                (when modified
                                  (concat
                                   " "
                                   (all-the-icons-faicon "floppy-o"
                                                         :face (when active 'error)
                                                         :v-adjust -0.01)))
                                " "
                                (powerline-buffer-id)
                                ))
                          (center (list
                                   " "
                                   (powerline-mode-icon)
                                   " "
                                   ;;major-mode
                                   (powerline-major-mode)
                                   " "))
                          (rhs (list
                                (format "%s" (eyebrowse--get 'current-slot))
                                " | "
                                (powerline-raw "%l:%c" face3 'r)
                                " | "
                                (powerline-raw "%6p" face3 'r)
                                (powerline-hud 'highlight 'region 1)
                                " "
                                ))
                          )
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face3 (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill 'mode-line-active (powerline-width rhs))
                      (powerline-render rhs))))))
  )
