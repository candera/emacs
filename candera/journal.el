;;; -*- lexical-binding: t; -*-
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Workday counting (excludes weekends and US federal holidays)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'calendar) ; for calendar-absolute-from-gregorian, etc.
(require 'holidays)

(defun candera:parse-iso-date (date)
  "Convert an ISO date string like \"2020-10-16\" to a Gregorian
list (MONTH DAY YEAR) as used by the calendar library. Also accepts
a value already in that list form and returns it unchanged."
  (if (listp date)
      date
    (let ((parts (mapcar #'string-to-number (split-string date "-"))))
      (list (nth 1 parts) (nth 2 parts) (nth 0 parts)))))

(defun candera:us-federal-holidays (year)
  "Return a list of Gregorian dates (MONTH DAY YEAR) for the US
federal holidays observed in YEAR. Fixed-date holidays that fall on a
weekend are shifted to the observed weekday (Saturday -> preceding
Friday, Sunday -> following Monday), which is when the day off is
actually taken."
  (let ((observed
         (lambda (month day)
           ;; Shift a fixed-date holiday to the observed weekday.
           (let* ((date (list month day year))
                  (dow (calendar-day-of-week date)))
             (cond
              ((= dow 0) (list month (1+ day) year))   ; Sunday -> Monday
              ((= dow 6) (list month (1- day) year))   ; Saturday -> Friday
              (t date))))))
    (list
     (funcall observed 1 1)                             ; New Year's Day
     (calendar-nth-named-day 3 1 1 year)               ; MLK Jr. Day (3rd Mon Jan)
     (calendar-nth-named-day 3 1 2 year)               ; Washington's Birthday (3rd Mon Feb)
     (calendar-nth-named-day -1 1 5 year)              ; Memorial Day (last Mon May)
     (funcall observed 6 19)                            ; Juneteenth
     (funcall observed 7 4)                             ; Independence Day
     (calendar-nth-named-day 1 1 9 year)               ; Labor Day (1st Mon Sep)
     (calendar-nth-named-day 2 1 10 year)              ; Columbus Day (2nd Mon Oct)
     (funcall observed 11 11)                           ; Veterans Day
     (calendar-nth-named-day 4 4 11 year)              ; Thanksgiving (4th Thu Nov)
     (funcall observed 12 25))))                        ; Christmas

(defun candera:us-holiday-absolute-days (start-year end-year)
  "Return a hash table whose keys are the absolute day numbers of all
US federal holidays observed in the years START-YEAR through END-YEAR
inclusive."
  (let ((table (make-hash-table :test 'eql)))
    (dolist (year (number-sequence start-year end-year))
      (dolist (date (candera:us-federal-holidays year))
        (puthash (calendar-absolute-from-gregorian date) t table)))
    table))

(defun candera:workdays-between (start-date end-date)
  "Return the number of workdays between START-DATE and END-DATE,
excluding weekends and US federal holidays. Dates are ISO strings like
\"2020-10-16\" (or Gregorian (MONTH DAY YEAR) lists).

The count is the number of workdays in the half-open interval
\[START-DATE, END-DATE): START-DATE is counted if it is a workday, and
END-DATE itself is not. If END-DATE precedes START-DATE the result is
negative."
  (interactive
   (list (read-string "Start date (YYYY-MM-DD): ")
         (read-string "End date (YYYY-MM-DD): ")))
  (let* ((start-greg (candera:parse-iso-date start-date))
         (end-greg (candera:parse-iso-date end-date))
         (start-abs (calendar-absolute-from-gregorian start-greg))
         (end-abs (calendar-absolute-from-gregorian end-greg))
         (lo (min start-abs end-abs))
         (hi (max start-abs end-abs))
         (holidays (candera:us-holiday-absolute-days
                    (nth 2 (calendar-gregorian-from-absolute lo))
                    (nth 2 (calendar-gregorian-from-absolute hi))))
         (count 0))
    (dolist (abs (number-sequence lo (1- hi)))
      (let ((dow (calendar-day-of-week (calendar-gregorian-from-absolute abs))))
        (when (and (/= dow 0)                 ; not Sunday
                   (/= dow 6)                 ; not Saturday
                   (not (gethash abs holidays)))
          (setq count (1+ count)))))
    (let ((result (if (> start-abs end-abs) (- count) count)))
      (when (called-interactively-p 'any)
        (message "%d workday%s" result (if (= (abs result) 1) "" "s")))
      result)))

(defvar candera:log-file-target-date
  ;; nil
  "2026-12-18"
  )

(defvar candera:log-file-target-type
  ;; nil
  "retirement"
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

(defun my-langtool-correct-buffer-safe ()
  "Run langtool-correct-buffer, skipping any dead overlays."
  (interactive)
  (let ((overlays (sort
                   (seq-filter
                    (lambda (ov)
                      (and (overlay-get ov 'langtool-message)
                           (overlay-get ov 'face)
                           (integerp (overlay-start ov))
                           (integerp (overlay-end ov))))
                    (overlays-in (point-min) (point-max)))
                   (lambda (a b)
                     (< (overlay-start a) (overlay-start b))))))
    (if overlays
        (let ((journal-langtool-enable-checking nil))
          (langtool--correction overlays))
      (message "No valid LangTool overlays found."))))

;; PERFORMANCE NOTE — first-run delay when the journal root is remote
;; (e.g. /scp:candera.sytes.net:daily/).
;;
;; The noticeable lag the first time this command runs in an Emacs session
;; comes from TWO sources, only one of which is TRAMP:
;;
;;   1. ~1.5s — TRAMP cold connection setup (SSH handshake + remote shell
;;      probing). Inherent to TRAMP; paid once per session, then the
;;      connection is reused, so re-running the command is fast.
;;
;;   2. ~1.0s — a hardcoded (sit-for 1 t) inside the built-in `after-find-file'.
;;      It is NOT a TRAMP round-trip. `after-find-file' finds that this file's
;;      auto-save file (a local /var/folders/.../T/#!scp:...# copy) exists and
;;      is NEWER than the remote file, so it prints
;;         "<file> has auto save data; consider M-x recover-this-file"
;;      and deliberately pauses one second so you can read it. Journal entries
;;      trigger this constantly because they get auto-saved locally but not
;;      always saved through to the remote, leaving a perpetually-newer
;;      auto-save file. This pause only occurs on the initial visit (later
;;      invocations just switch to the existing buffer).
;;
;; Ruled out as causes: VC backend detection, dir-locals lookup, GC, and
;; undo-tree's remote .~undo-tree~ history writes (extra round-trips, but not
;; the source of the delay).
;;
;; The ~1s pause is eliminable by opening via (find-file-noselect FILE t) —
;; the NOWARN arg makes `after-find-file' skip the warning and the sit-for —
;; at the cost of losing the recover-this-file hint for journal files.
;; (Left unchanged intentionally.)
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
	 (lsp-bridge-enable-predicates (when (fboundp 'lsp-bridge-enable-predicates)
					 (append lsp-bridge-enable-predicates
						 (lambda () nil)))))
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
				(format "\n\n%d days (%d work days) remaining%s."
						(days-to-date logfile-date candera:log-file-target-date)
						(candera:workdays-between
						 (format-time-string "%Y-%m-%d" logfile-date)
						 candera:log-file-target-date)
					(when candera:log-file-target-type
					  (concat " until " candera:log-file-target-type)))
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
          (auto-fill-mode 0)
	  (setq fill-column 80)
	  (visual-line-mode 1)
	  (visual-fill-column-mode 1)
          (setq show-trailing-whitespace t)
	  (setq buffer-read-only nil)
	  (setq-local company-idle-delay nil)

	  (keymap-local-set "M-;" 'journal-langtool-correct-previous)
	  (keymap-local-set "M-'" 'my-langtool-correct-buffer-safe)
	  
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

