;;; org-checklist-rollup.el --- Show a "Checklist" subheading's progress on its parent headline -*- lexical-binding: t; -*-

;; Org headline cookies like [2/5] only count checkboxes in the heading's
;; own text, not those under a child heading.  When a task keeps its
;; checkboxes under a "*** Checklist [/]" subheading, this copies that
;; subheading's count to a cookie on the task's headline, so the progress
;; shows on the agenda line:
;;
;;   ** TODO Investigate silver disk options [2/5]
;;   *** Checklist [2/5]
;;
;; Opt in per file with a file-local variable (no confirmation prompt):
;;
;;   # Local Variables:
;;   # candera/org-checklist-rollup: t
;;   # End:
;;
;; The parent needs a [/] or [%] cookie in its headline to begin with, and
;; the Checklist subheading needs a [/] cookie (org keeps that one current).

(require 'org)

(defvar-local candera/org-checklist-rollup nil
  "When non-nil, mirror each Checklist subheading's count onto its parent headline.")
(put 'candera/org-checklist-rollup 'safe-local-variable #'booleanp)

(defun candera/org-checklist-rollup-update ()
  "Copy each Checklist subheading's [done/total] onto its parent's headline cookie."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) .*?\\(\\[[0-9]*\\(?:/[0-9]*\\|%\\)\\]\\)" nil t)
        (let* ((level (length (match-string 1)))
               (cookie-beg (match-beginning 2))
               (cookie-end (match-end 2))
               (old (match-string-no-properties 2))
               (resume (line-end-position))
               (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
               (child (save-excursion
                        (goto-char resume)
                        (when (re-search-forward
                               (format "^\\*\\{%d\\} Checklist.*\\[\\([0-9]+\\)/\\([0-9]+\\)\\]"
                                       (1+ level))
                               subtree-end t)
                          (cons (string-to-number (match-string 1))
                                (string-to-number (match-string 2)))))))
          (when child
            (let ((new (if (string-suffix-p "%]" old)
                           (format "[%d%%]" (if (zerop (cdr child)) 0
                                              (/ (* 100 (car child)) (cdr child))))
                         (format "[%d/%d]" (car child) (cdr child)))))
              (unless (string= old new)
                (goto-char cookie-beg)
                (delete-region cookie-beg cookie-end)
                (insert new))))
          (goto-char resume))))))

(defun candera/org-checklist-rollup--after-count (&rest _)
  "Advice: roll up after Org refreshes checkbox cookies."
  (when candera/org-checklist-rollup
    (candera/org-checklist-rollup-update)))

(defun candera/org-checklist-rollup--before-save ()
  "Refresh all checkbox cookies, then roll up, before saving."
  (when (and candera/org-checklist-rollup (derived-mode-p 'org-mode))
    (org-update-checkbox-count t)
    (candera/org-checklist-rollup-update)))

;; Both are no-ops in buffers that haven't opted in.
(advice-add 'org-update-checkbox-count :after #'candera/org-checklist-rollup--after-count)
(add-hook 'before-save-hook #'candera/org-checklist-rollup--before-save)

(provide 'org-checklist-rollup)
;;; org-checklist-rollup.el ends here
