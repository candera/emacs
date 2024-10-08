;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up automatic journal file creation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *journal-roots* '()) ; new entries must end with slash

(defun days-to-date (now-date to-date)
  "Returns the number of days until a date. Input format is 2020-10-16"
  (truncate
   (/ (- (float-time (date-to-time (format "%sT00:00" to-date)))
         (float-time now-date))
      (* 60 60 24))))

(defun display-days-to-date (date)
  "Returns the number of days until a date. Input format is 2020-10-16"
  (interactive "MDate: ")
  (message (format "%d" (days-to-date (time-n-days-ago 0) date))))

(defvar candera:log-file-target-date
  nil
  ;; "2023-06-30"
  )

(defvar journal-buffer-last-hash nil)

(make-variable-buffer-local 'journal-buffer-last-hash)

(defvar journal-langtool-enable-checking t)

(make-variable-buffer-local 'journal-langtool-enable-checking)

(defun journal-langtool-correct-previous ()
  (interactive)
  (let ((journal-langtool-enable-checking nil))
    (save-excursion
      (langtool-goto-previous-error)
      (forward-char)
      (langtool-correct-at-point)
      (langtool-check-done))))

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
           (concat new-logfile-directory "/%Y%m%d.txt") logfile-date))
	 (lsp-bridge-enable-predicates (append lsp-bridge-enable-predicates
					       (lambda () nil))))
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
                            (format-time-string ", %Y." logfile-date)
			    (if candera:log-file-target-date
				(format "\n\n%d days remaining." (days-to-date logfile-date candera:log-file-target-date))
			      "")
			    ))
            ;; Auto save over SSH is a PITA. This will still auto-save
            ;; on idle.
            (when (or (numberp (string-match "/ssh:" new-logfile-filename))
                      (numberp (string-match "/scp:" new-logfile-filename)))
              (set-variable 'auto-save-interval 0))
            (newline)
            (newline)
            (newline)
            (previous-line)
            (message (concat "Opened " new-logfile-filename)))
	  (text-mode)
          (flyspell-mode 1)
          (auto-fill-mode 1)
          (setq show-trailing-whitespace t)
	  (setq buffer-read-only nil)
	  (setq-local company-idle-delay nil)

	  (keymap-local-set "M-;" 'journal-langtool-correct-previous)
	  
	  (lexical-let* ((this-buffer (current-buffer))
			 (timer (run-with-idle-timer
				 2
				 t
				 (lambda ()
				   (lexical-let ((h (buffer-hash (current-buffer))))
				     (when (and (eq this-buffer (current-buffer))
						(not (eq h journal-buffer-last-hash))
						journal-langtool-enable-checking)
				       (setq journal-buffer-last-hash h)
				       (langtool-check-buffer)))))))

	    (add-hook 'kill-buffer-hook
		      (lambda ()
			(when (eq this-buffer (current-buffer))
			  (cancel-timer timer))))
	    ))))))

(defun find-random-log-file ()
  (interactive)
  (find-yesterday-log-file (random (days-between (concat (format-time-string "%F") "T00:00")
                                                 "1993-07-10T00:00"))))

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
      (if (condition-case
	      _ (file-directory-p (expand-file-name (car journal-roots)))
	    (error nil))
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

