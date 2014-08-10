
(defun org-html-slideshow-presentation-narrow-to-current-slide ()
  "Narrows the active buffer to the nearest ancestor headline
  containing point and marked with the slide tag"
  (interactive)
  (org-head))

(defun turn-on-org-html-slideshow-presentation-mode ()
  "Turns on org-html-slideshow-presentation-mode"
  (interactive)
  (org-html-slideshow-presentation-narrow-to-current-slide)
  (message "Turning on org-html-slideshow-presentation-mode"))

(defun turn-off-org-html-slideshow-presentation-mode
  (message "Turning off org-html-slideshow-presentation-mode"))

(define-minor-mode org-html-slideshow-presentation-mode
  "Minor mode for presenting org files that use `org-html-slideshow`."
  :init-value t
  :lighter " Present"
  :keymap (make-keymap "org-html-slideshow-presentation")
  (if org-html-slideshow-presentation-mode
      (turn-on-org-html-slideshow-presentation-mode)
    (turn-off-org-html-slideshow-presentation-mode)))

(provide 'org-html-slideshow-presentation)

