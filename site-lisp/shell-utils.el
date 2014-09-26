;;; Creates a shell with the project root dir as the buffer name
(defun last-dir (abs-dir)
  (first (last (delq nil
                     (mapcar (lambda (x) (and (not (string= x "")) x))
                             (split-string default-directory "/"))))))
(defun named-shell (type)
  (interactive "sType: ")
  (let ((shell-type (if (string= type "") "shell" type)))
    (with-project-root
        (shell (concat "*" (last-dir default-directory) "-" shell-type "*")))))





