;;;; -*- Mode: Emacs-Lisp -*-

;;; This file implements a set of functions for replacing symbols
;;; within a sexp or a buffer. A symbol for this purpose is anything
;;; for which (progn (forward-sexp) (backward-sexp)) is idempotent and
;;; for which (progn (down-list) (backward-up-list)) is *not*
;;; idempotent. It is case sensitive at the moment, but that may
;;; change in the future.

;;; I have used this to rename variables, functions, types, &c. in
;;; both Lisp and C. Your mileage may of course vary.

;;; This file was written by Brian Mastenbrook (brian AT mastenbrook
;;; DOT net) and is placed in the public domain.

;;; M-x replace-symbol-in-sexp from to
;;; M-x replace-symbol-in-buffer from to

(defun replace-symbol-in-sexps-until-error (from to)
  ;; used for replacing all the sexps inside of a list. when a
  ;; scan-error is caught, we return.
  (condition-case c
      (while t
        (replace-symbol-in-sexp from to t)
        (forward-sexp))
    (scan-error nil)))

(defun beginning-of-list-p ()
  (save-excursion
    (condition-case c
        (progn
          (forward-sexp)
          (backward-sexp)
          (let ((point-1 (point)))
           (down-list)
           (backward-up-list)
           (forward-sexp)
           (backward-sexp)
           (eq (point) point-1)))
      (scan-error nil))))

(defun replace-symbol-in-sexp (from to &optional recursive)
  (interactive "sReplace symbol: \nsReplace symbol %s with: ")
  (let ((do-replace
         (lambda ()
           (save-excursion
             (forward-sexp)
             (backward-sexp)
             ;; the combination of those two puts us on the first character
             ;; of the sexp
             (cond
              ((beginning-of-list-p)
               ;; we're sitting on the beginning of a list
               (down-list)   ; travel into the sexp and replace inside
               (replace-symbol-in-sexps-until-error from to))
              (t (let ((beginning-of-sexp (point))
                       (end-of-sexp 0))
                   (forward-sexp)
                   (setq end-of-sexp (point))
                   (backward-sexp)
                   ;; we get these values for the beginning and end of the
                   ;; sexp so we can compare and delete it
                   (if (string-equal (buffer-substring beginning-of-sexp
                                                       end-of-sexp) from)
                       (progn
                         (delete-region beginning-of-sexp end-of-sexp)
                         (insert to)
                         (setq replaced-in-sexp (1+ replaced-in-sexp)))
                     nil))))))))
    (if recursive
        (funcall do-replace)
      (let ((replaced-in-sexp 0))
        (funcall do-replace)
        (unless recursive
          (message "Replaced %s occurrence%s"
                   replaced-in-sexp
                   (if (eq replaced-in-sexp 1) "" "s")))))))

(defun replace-symbol-in-buffer (from to)
  (interactive "sReplace symbol: \nsReplace symbol %s with: ")
  (let ((replaced-in-sexp 0))
    (save-excursion
      (goto-char (point-min))
      (condition-case c
          (while (not (eq (point-max) (point)))
            (replace-symbol-in-sexp from to t)
            (forward-sexp))
        (scan-error nil)))
    (message "Replaced %s occurrence%s"
             replaced-in-sexp
             (if (eq replaced-in-sexp 1) "" "s"))))

(provide 'refactor)