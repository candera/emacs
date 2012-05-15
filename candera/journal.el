;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up automatic journal file creation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *journal-roots* '()) ; new entries must end with slash

(defun find-yesterday-log-file (&optional days-ago)
  "Open a file that has the default settings for yesterday's entry"
  (interactive "p")
  (let ((*journal-roots*
         (if (null *journal-roots*)
             (list (read-directory-name "Base directory: "))
           *journal-roots*)))
    (let*
        ((n-days-ago (if (null days-ago) 1 days-ago))
         (logfile-date (time-n-days-ago n-days-ago))
         (logfile-directory (available-logfile-directory *journal-roots*))
         (new-logfile-directory (format-time-string (concat logfile-directory "%Y/%m-%b") logfile-date))
         (new-logfile-filename
          (format-time-string
           (concat new-logfile-directory "/%Y%m%d.txt") logfile-date)))
      (progn
        (make-directory new-logfile-directory t)
        (let ((existing?
               (or (find-buffer-visiting new-logfile-filename)
                   (file-exists-p new-logfile-filename))))
          (find-file new-logfile-filename)
          (unless existing?
            (insert (concat (format-time-string "%A, %B " logfile-date)
                            (day-of-month-ordinal
                             (string-to-number
                              (format-time-string "%e" logfile-date)))
                            (format-time-string ", %Y." logfile-date)))
            (newline)
            (newline)
            (newline)
            (previous-line)
            (message (concat "Opened " new-logfile-filename)))
          (flyspell-mode 1)
          (auto-fill-mode 1)
          (setq show-trailing-whitespace t))))))

;; (defun days-ago (n)
;;   "Returns a value similar to current-time, but for n days ago"
;;   (interactive)
;;   (let ((now (float-time)))
;;     (

(defun time-n-days-ago (n)
  "Returns a value similar to get-float-time, but for n days ago"
  (interactive)
  (seconds-to-time (- (float-time) (* 24 3600 n))))

(defun available-logfile-directory (journal-roots)
  "Returns the first available directory from the list journal-roots"
  (interactive)
  (if journal-roots
      (if (or (file-directory-p (expand-file-name (car journal-roots))))
          (expand-file-name (car journal-roots))
        (available-logfile-directory (cdr journal-roots)))
    nil))

(defun day-of-month-ordinal (n)
   "Returns ordinal for range 1-31 (1st, 2nd, 3rd, etc.)"
   ()
   (if (= n 1) "1st"
     (if (= n 2) "2nd"
       (if (= n 3) "3rd"
         (if (= n 21) "21st"
           (if (= n 22) "22nd"
             (if (= n 23) "23rd"
               (if (= n 31) "31st"
                 (if (and (> n 0) (< n 31)) (concat (number-to-string n) "th") nil)))))))))

