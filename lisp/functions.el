;;; functions.el --- miscellaneous function definitions

;;; Commentary: 
;;; author: ccann

;;; Code: 

;; pulled these from someone... don't remember who!
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

;;; functions.el ends here
