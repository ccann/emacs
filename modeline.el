;; Courtesy of https://github.com/MaxSt/challenger-deep/issues/1
;; (use-package rainbow-mode)
(use-package all-the-icons)
;; (use-package eyebrowse)
(use-package powerline
  :config

  (defun custom-modeline-github-vc ()
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (concat
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.2 :family ,(all-the-icons-octicon-family))
                   'display '(raise -0.1))
       (propertize (format " %s" branch) 'face `(:height 1.0)))))

  
  (defun custom-modeline-icon-vc ()
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode) (custom-modeline-github-vc))
       (t (format "%s" vc-mode)))))


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
                          (face1 (if active 'mode-line 'mode-line-inactive))
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
                                " | "
                                (powerline-raw "%l:%c" 'face1 'r)))
                          (center (list
                                   " "
                                   (powerline-mode-icon)
                                   " "
                                   (powerline-major-mode)
                                   " "))
                          (rhs (list
                                (custom-modeline-icon-vc)
                                ;; (format "%s" (eyebrowse--get 'current-slot))
                                " | "
                                (powerline-raw "%6p" 'face1 'r)
                                (powerline-hud 'highlight 'region 1)
                                " ")))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center 'face1 (/ (powerline-width center) 2.0))
                      (powerline-render center)
                      (powerline-fill 'face1 (powerline-width rhs))
                      (powerline-render rhs)))))))
