;;; functions.el --- miscellaneous function definitions

;;; Commentary: 
;;; author: ccann

;;; Code: 

;;;###autoload
(progn
  
  ;; pulled these from someone... don't remember who!
  (defun unfill-paragraph ()
    (interactive)
    (let ((fill-column 90002000))
      (fill-paragraph nil)))
  
  (defun unfill-region (start end)
    (interactive "r")
    (let ((fill-column 90002000))
      (fill-region start end)))
  
  (defun find-user-init-file ()
    "Edit the `user-init-file', in another window."
    (interactive)
    (find-file-other-window user-init-file)))

(provide 'functions)
;;; functions.el ends here
