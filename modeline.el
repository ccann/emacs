;;; modeline.el --- Modeline implementation

;;; Commentary:
;;; adapted from https://github.com/MaxSt/challenger-deep/issues/1

;;; Code:

(use-package all-the-icons)
(use-package powerline)

(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-"))
        (face (if (powerline-selected-window-active) 'font-lock-warning-face 'mode-line-inactive)))
      (concat
       ;; (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
       "  "
       (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1.3 :family ,(all-the-icons-octicon-family)
                                   :inherit ,face)
                   'display '(raise -0.1))
       (propertize (format " %s" branch)
                   'face
                   `(:height 0.9 :inherit ,face)))))

(defun projectile-root ()
    "Show the current projectile root."
    (if (and (fboundp 'projectile-project-p)
             (stringp (projectile-project-p))
             (not (string= (projectile-project-name) (buffer-name))))
        (format "[%s] " (projectile-project-name))
      ""))

(defun custom-modeline-icon-vc ()
    (when vc-mode
      (cond
       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
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
        (format
         " %s"
         (propertize icon
                     'help-echo (format "Major-mode: `%s`" major-mode)
                     'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active (powerline-selected-window-active))
           (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-inactive))
           (modified (buffer-modified-p))
           (mode-line (if active 'mode-line 'mode-line-inactive))
           (bar-color (cond ((and active modified) (face-foreground 'error))
                            (active (face-background 'cursor))
                            (t (face-background 'tooltip))))
           (mm-face (if active 'font-lock-keyword-face 'mode-line-inactive))
           (separator (powerline-raw " | " mode-line))
           (space (powerline-raw " " mode-line))
           (hud-face1 (if active 'highlight 'mode-line-inactive))
           (hud-face2 (if active 'region 'mode-line-inactive))
           (lhs (list (make-rect bar-color 30 3)
                      (when modified
                        (concat
                         space
                         (all-the-icons-faicon "floppy-o"
                                               :face (if active 'error 'mode-line-inactive)
                                               :v-adjust -0.01)))
                      space
                      (powerline-raw (projectile-root) mode-line)
                      (powerline-buffer-id mode-line-buffer-id)
                      (custom-modeline-icon-vc)))
           (center (list space
                         (powerline-mode-icon)
                         (powerline-major-mode mm-face 'l)
                         (powerline-process)
                         space))
           (rhs (list
                 ;; (powerline-raw (format "%s" (eyebrowse--get 'current-slot)) mode-line)
                 ;; separator
                 (powerline-raw "%l:%c" mode-line)
                 space
                 ;; separator
                 (powerline-raw "%6p" mode-line 'r)
                 (powerline-hud hud-face1 hud-face2)
                 space)))
      (concat (powerline-render lhs)
              (powerline-fill-center mode-line (/ (powerline-width center) 2.0))
              (powerline-render center)
              (powerline-fill mode-line (powerline-width rhs))
              (powerline-render rhs))))))
