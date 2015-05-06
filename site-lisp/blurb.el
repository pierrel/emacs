(defvar *blurb-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b j") 'blurb-wip-set)
    (define-key map (kbd "C-c b w") 'blurb-wip-get)
    map))

(defvar *blurb-wip-story* nil)
(defvar *blurb-wip-story-desc* nil)
(defun blurb-wip-set (&optional story description)
  (interactive "sstory:\nsdescription:")
  (setq *blurb-wip-story* story)
  (setq *blurb-wip-story-desc* description))
(defun blurb-wip-get ()
  (interactive)
  (if *blurb-wip-story*
      (progn
        (insert *blurb-wip-story*)
        (message (format "%s: %s" *blurb-wip-story* *blurb-wip-story-desc*)))
    (progn
      (call-interactively 'blurb-wip-set)
      (call-interactively 'blurb-wip-get))))

(define-minor-mode blurb-mode "Bindings and tasks to deal with blurb"
  :lighter " blurb" :global t :keymap *blurb-mode-map*)

(defun blurb-mode ()
  (interactive)
  (unless (assoc 'blurb-mode minor-mode-map-alist)
    (add-to-list 'minor-mode-map-alist (cons 'blurb-mode blurb-mode-map)))
  (setq blurb-mode (not blurb-mode)))


(provide 'blurb)
