(defun load-file-if-exists (path)
  (if (file-exists-p path)
      (load-file path)))

;; Disable vc for files opened with TRAMP, because it's slow and I
;; neve really want to do that.
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defmacro comment (&rest body)
       "Comment out one or more s-expressions."
         nil)

;; Add the following to your init file to have packages installed by
;; Homebrew added to your load-path:
;; (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-archives
 (append package-archives
         '(("melpa" . "http://melpa.org/packages/")
           ("melpa-stable" . "http://stable.melpa.org/packages/")
           ("org" . "http://orgmode.org/elpa/"))))
(when (< emacs-major-version 27)
  (package-initialize))

;; Install straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Bug in 28 means that SVG is inadvertantly not included
(when (= emacs-major-version 28)
  (setq iamge-types (append image-types '(svg))))

(unless (fboundp 'use-package)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; el-get
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package el-get
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-mode
;; 
;; OMFG: cider requires org-mode, which will pull in the default
;; version built in to Emacs if I don't set it up before
;; clojure-mode/cider.
;;
;; Set up later version of org-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-plus-contrib
;;   :pin org)

(defun setup-org-mode ()
  (turn-on-flyspell)
  (auto-fill-mode 1)
  ;; I always type this instead of C-c C-t
  (auto-revert-mode 1)
  (add-to-list 'org-modules 'org-habit)
  (when (fboundp 'lsp-bridge-mode)
    (lsp-bridge-mode -1))
  ;; Org files can get big and have lots of
  ;; folded content. There's not much benefit
  ;; in line numbers, and they slow down org
  ;; noticably.
  ;; (linum-mode 0)
  ;; Weird that I have to do this, but I
  ;; can't figure out how to get habits
  ;; turned on outside of the customization
  ;; interface, which I prefer not to use.
  (require 'org-habit)
  ;; Automatically save org-mode buffers when
  ;; idle. Important because I use Dropbox to
  ;; sync them, and forgetting to save when
  ;; switching computers means conflicts.
  (add-to-list 'idle-save-buffer-list (current-buffer))
  ;; Don't populate the initial input with
  ;; "^" when refiling - allows me just to
  ;; type what I'm after without having it
  ;; match the beginning of the string.
  (delete '(org-refile . "^") ivy-initial-inputs-alist)
  (delete '(org-agenda-refile . "^") ivy-initial-inputs-alist)
  (delete '(org-capture-refile . "^") ivy-initial-inputs-alist)
  (add-to-list 'ivy-initial-inputs-alist '(org-refile . ""))
  (add-to-list 'ivy-initial-inputs-alist '(org-agenda-refile . ""))
  (add-to-list 'ivy-initial-inputs-alist '(org-capture-refile . "")))

(defun setup-org-agenda-mode ()
  ;; I always type this instead of C-c C-t
  (define-key org-agenda-mode-map (kbd "C-c t") 'org-agenda-todo)
  (display-line-numbers-mode 0))

(use-package org
  :ensure t
  :pin gnu

  :config
  ;; (global-set-key (kbd "C-c a") 'org-agenda-view-mode-dispatch)
  (global-set-key (kbd "C-c l") 'org-store-link) 
  ;; ;; Bizarrely, org-clock defaults to showing the current year only
  ;; (setq org-clock-display-default-range 'untilnow)
  ;; This requests logging when going from TODO to INPROGRESS and from INPROGRESS to DONE
  (setq org-todo-keywords (quote ((sequence "TODO(t!)" "INPROGRESS(i!)" "PAUSED(p@)" "BLOCKED(b@)" "DONE(d!)"))))
 
  :hook
  ((org-mode . setup-org-mode)
   (org-agenda-mode . setup-org-agenda-mode))

  :bind
  (:map org-mode-map
	("C-c w" . org-refile-goto-last-stored)
	("C-c t" . org-todo)
	("M-." . org-open-at-point)
	("M-," . org-mark-ring-goto)
	("C-c a" . org-agenda)
	;; (define-key org-mode-map (kbd "H-g") 'counsel-org-goto)

	))


;; Skip items with the noagenda tag
(defun org-init-skip-tags ()
  "Skip the \"noagenda\" tags."
  (let ((tags (org-get-tags-at (point))))
    (when (member "noagenda" tags)
      (save-excursion
        (or
         (ignore-errors (org-forward-element)
                        (point))
         (point-max))))))
(setq org-agenda-skip-function-global 'org-init-skip-tags)

(global-set-key
 (kbd "C-c a")
 (lambda ()
   (interactive)
   (if (get-buffer "*Org Agenda*")
       (switch-to-buffer "*Org Agenda*")
     (org-agenda))))

;; ;; org-mode refuses to invoke org-indent-mode in emacs 23, claiming
;; ;; that it might crash. So I set this variable, which gets me the same
;; ;; effect.
;; (setq org-hide-leading-stars t)

;; Allow slash-separated paths when refiling
;; (setq org-refile-use-outline-path 'file)
;; (setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets  
      '(
	;; (org-agenda-files . (:tag . "active"))
	(org-agenda-files . (:maxlevel . 3))
	)
      )

;; Ivy breaks org-refile for some reason
(defadvice org-refile-disable-ivy (around org-refile)
  (ivy-mode 0)
  (let ((result ad-do-it))
    (ivy-mode 1)
    result))

;; ;; Log into a drawer, which is nice
;; (setq org-log-into-drawer t)

;; Include things in the diary file
(setq org-agenda-include-diary t)

;; Make events sort by newest first in the agenda view
(setq org-agenda-sorting-strategy
      '((agenda priority-down timestamp-down habit-down time-up category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

;; Store captured notes in notes.org, and bind capture to C-c c
(setq org-default-notes-file "~/notes.org")
(define-key global-map (kbd "C-c c") 'org-capture)

;; ;; Log time task was closed
(setq org-log-done t)

(setq org-agenda-log-mode-items '(closed clock state))

;; ;; Turn off the annoying mouse highlight in agenda views
;; (add-hook 'org-finalize-agenda-hook
;;           (lambda () (remove-text-properties
;;                       (point-min) (point-max) '(mouse-face t))))

;; ;; Set up for agendas and mobile org
;; (when (file-exists-p "~/Dropbox/org/")
;;   ;; Set to the location of your Org files on your local system
;;   (setq org-directory "~/Dropbox/org/")
;;   ;; Set to the name of the file where new notes will be stored
;;   (setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
;;   ;; Set to <your Dropbox root directory>/MobileOrg.
;;   (setq org-mobile-directory "~/Dropbox/MobileOrg")
;;   ;; A file that lists which org files should be pulled into the agenda
;;   (setq org-agenda-files "~/Dropbox/org/agendas.org"))

(defun org-time-difference (ts1 ts2)
  "Given two org time strings, return the floating point time
  difference between them."
  (let* ((time1 (org-time-string-to-time ts1))
         (time2 (org-time-string-to-time ts2))
         (t1 (org-float-time time1))
         (t2 (org-float-time time2)))
    (- t2 t1)))

(defun org-custom-todo-sort-fn ()
  "Returns a value that sorts tasks according to my personal
heuristic. Namely, by task state: INPROGRESS, then BLOCKED, then
TODO, then PAUSED, then nothing, then DONE. Within the non-done
states, sort by scheduled, or by deadline if not scheduled, with
oldest dates first. Within in-progress tasks, sort by priority
before scheduled. Within DONE, most-recently done first. Archived
items are always last."
  (format "%s/%s-%s/%s"
          (if (member "ARCHIVE" (org-get-tags)) "1" "0")
          (pcase (org-get-todo-state)
            ("INPROGRESS" (format "1%s" (or (org-entry-get (point) "PRIORITY") "Z")))
            ("BLOCKED" 2)
            ("TODO" (format "3%s" (or (org-entry-get (point) "PRIORITY") "Z")))
	    ("PAUSED" 4)
            (`nil 5)
            ("DONE" (format "6%20d" (let ((ct (org-entry-get (point) "CLOSED")))
                                      (if ct
                                          (org-time-difference ct "2038-01-01")
                                        1.0e23))))
            (otherwise 9))
          (let ((etime (or (org-entry-get (point) "SCHEDULED")
                           (org-entry-get (point) "DEADLINE"))))
            (if etime
                (format "%013.2f" (org-float-time (org-time-string-to-time etime)))
              "zzzzzzzzzzzzzz"))

          (org-get-heading :no-tags :no-todo)))

(defun org-custom-entry-sort ()
  "Sorts entries according to my personal heuristic"
  (interactive)
  (org-sort-entries nil ?f 'org-custom-todo-sort-fn))

;; ;; Compute my own custom scores for habits

;; (defun org-candera-habit-penalty (days-since-last)
;;   (- (if days-since-last
;;          (if (< 1 days-since-last)
;;              (- (expt 2 days-since-last)
;;                 2)
;;            0)
;;        0)))

;; (defun org-candera-habit-score (days today initial-value)
;;   (when days
;;     (let ((score-info
;;            (reduce (lambda (acc d)
;;                      (let* ((last-day (gethash :last-day acc))
;;                             (days-since-last (when last-day (- d last-day)))
;;                             (on-streak? (when days-since-last (= 1 days-since-last)))
;;                             (streak (if on-streak? (1+ (gethash :streak acc 0)) 1))
;;                             (streak-bonus (if on-streak?
;;                                               (if (zerop (mod streak 3))
;;                                                   (/ streak 3)
;;                                                 0)
;;                                             0))
;;                             (score (gethash :score acc 0)))
;;                        (puthash :last-day d acc)
;;                        (puthash :streak streak acc)
;;                        (puthash :score (max 1 (+ (or score 0)
;;                                                  streak-bonus
;;                                                  1
;;                                                  (org-candera-habit-penalty days-since-last)))
;;                                 acc)
;;                        acc))
;;                    days
;;                    :initial-value initial-value)))
;;       (let ((today-penalty (org-candera-habit-penalty (- (1+ today)
;;                                                          (first (last days))))))
;;         (puthash :score
;;                  (max 0 (+ (gethash :score score-info) today-penalty))
;;                  score-info)
;;         (unless (zerop today-penalty)
;;           (puthash :streak 0 score-info))
;;         score-info))))

;; (defun org-collect-dates-for-element ()
;;   "Gets all the dates for the element at point"
;;   (let* ((element (org-element-at-point))
;;          (start (org-element-property :begin element))
;;          (end (org-element-property :end element)))
;;     (org-get-all-dates start end nil nil t)))

;; (defun org-collect-dates (match)
;;   "Returns all the unique dates that appear in items that match MATCH"
;;   ;; TODO: Figure how to keep it from scanning both parents and
;;   ;; children, since that's redundant
;;   ;; TODO: Skipping archived items doesn't seem to work,
;;   ;; although skipping commented items does.
;;   (let* ((dates (apply #'append
;;                        (org-map-entries #'org-collect-dates-for-element
;;                                         match
;;                                         'file
;;                                         'archive
;;                                         'comment)))
;;          (uniques (cl-remove-duplicates dates))
;;          (sorted ))
;;     (cl-sort uniques #'<)))

;; (defun org-dblock-write:compute-habit-score (params)
;;   "Returns a 'score' for entries that match `match` (e.g. a tag)
;;   based on timestamps that appear in them.

;;   One point is given for each consecutive day that appears. A day
;;   without activity drops the score by (expt 2
;;   days-since-last-activity). Every third day of a streak, a bonus
;;   of (/ streak-length 3) is awarded.

;;   If not all data is recorded in the org file initially, initial
;;   values can be provided via :last-day, :initial-streak,
;;   and :initial-score params."
;;   (interactive)
;;   (let* ((last-day-param (plist-get params :last-day))
;;          (last-day (when last-day-param
;;                      (time-to-days (org-time-string-to-time last-day-param))))
;;          (initial-streak (plist-get params :initial-streak))
;;          (initial-score (plist-get params :initial-score))
;;          (initial-value (make-hash-table))
;;          (match (plist-get params :match)))
;;     (puthash :last-day last-day initial-value)
;;     (puthash :streak initial-streak initial-value)
;;     (puthash :score initial-score initial-value)
;;     (save-excursion
;;       (let* ((dates (org-collect-dates match))
;;              (score-info (org-candera-habit-score
;;                           (if last-day
;;                               (remove-if (lambda (d) (<= d last-day)) dates)
;;                             dates)
;;                           (time-to-days (current-time))
;;                           initial-value)))
;;         (insert
;;          (format "Score as of %s: %s\nStreak: %d"
;;                  (format-time-string "%Y-%m-%d" (current-time))
;;                  (if score-info (or (gethash :score score-info) "No score") "No score")
;;                  (if score-info (or (gethash :streak score-info) 0) 0)))))))

;; (defun candera:goal-achieved?
;;   (achieved?)
;;   (string-prefix-p achieved? "y" t))

;; (defun candera:streak-game-compute-elapsed
;;   (current-date prior-date current-achievement prior-achievement prior-elapsed)
;;   (let ((date-difference (floor
;;                           (/ (org-time-difference current-date prior-date)
;;                              (* 24 60 60.0)))))
;;     (if (candera:goal-achieved? current-achievement)
;;         (+ prior-elapsed date-difference)))
;;   )

;; ;; I don't want to see days in cumulative durations, thanks
;; (setq org-time-clocksum-format
;;       '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;; cl is deprecated as of Emacs 27
(require 'cl-lib)

;; ;; At some point, things started breaking on absence of this symbol,
;; ;; probably because it got added to emacs 27. But emacs 27 isn't
;; ;; working so great for me at the moment.
;; (unless (fboundp 'flatten-tree)
;;   (defun flatten-tree (tree)
;;     "Take TREE and \"flatten\" it.
;; This always returns a list containing all the terminal nodes, or
;; \"leaves\", of TREE.  Dotted pairs are flattened as well, and nil
;; elements are removed.
;; \(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
;; => (1 2 3 4 5 6 7)
;; TREE can be anything that can be made into a list.  For each
;; element in TREE, if it is a cons cell return its car
;; recursively.  Otherwise return the element."
;;     (let (elems)
;;       (setq tree (list tree))
;;       (while (let ((elem (pop tree)))
;;                (cond ((consp elem)
;;                       (setq tree (cons (car elem) (cons (cdr elem) tree))))
;;                      (elem
;;                       (push elem elems)))
;;                tree))
;;       (nreverse elems)))

;;   ;; Technically, `flatten-list' is a misnomer, but we provide it here
;;   ;; for discoverability:
;;   (defalias 'flatten-list 'flatten-tree))

;; ;; This would be great if it didn't just cause cider to completely disappear
;; ;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; ;; Someone broke org-mobile. We have to load from a fixed copy.
;; ;; (setq load-path (append '("~/.emacs.d/custom/org-mode/lisp"
;; ;;                           "~/.emacs.d/custom/org-mode/contrib/lisp")
;; ;;                         load-path))

;; ;; Fix garbage collection so that it doesn't happen when the
;; ;; minibuffer is open. See
;; ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold 800000))

;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(require 'calendar)  ;; for calendar-absolute-from-gregorian

(defface my-org-agenda-current-block-face
  '((t :background "gray20"))
  "Face used to highlight the currently active scheduled block in the agenda.")

(defun my-org-agenda-highlight-current-block ()
  "Highlight today's agenda item whose time range includes the current time.

Only touches lines for the current day, highlights the whole line,
and shows a leading * without changing indentation."
  (when (eq org-agenda-type 'agenda)
    (let* ((today-abs (org-today))
           (now-minutes (let ((hh (string-to-number (format-time-string "%H")))
                              (mm (string-to-number (format-time-string "%M"))))
                          (+ (* hh 60) mm))))
      ;; Clear older highlights (line + star overlays)
      (remove-overlays (point-min) (point-max) 'my-current-block t)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((date-prop  (get-text-property (point) 'date))
                 (date-abs   (when (consp date-prop)
                               (calendar-absolute-from-gregorian date-prop)))
                 (start-hhmm (get-text-property (point) 'time-of-day))
                 (duration   (get-text-property (point) 'duration)))
            ;; Only consider lines belonging to *today* with a time + duration
            (when (and date-abs
                       (= date-abs today-abs)
                       start-hhmm
                       duration)
              (let* ((hh (/ start-hhmm 100))
                     (mm (% start-hhmm 100))
                     (start-minutes (+ (* hh 60) mm))
                     (end-minutes   (+ start-minutes duration)))
                (when (and (>= now-minutes start-minutes)
                           (<  now-minutes end-minutes))
                  ;; 1) Highlight the whole line
                  (let* ((line-beg (line-beginning-position))
                         (line-end (line-end-position))
                         (ov      (make-overlay line-beg line-end)))
                    (overlay-put ov 'face 'my-org-agenda-current-block-face)
                    (overlay-put ov 'my-current-block t))
                  ;; 2) Replace the first character (usually a space) with "*"
                  (let* ((line-beg (line-beginning-position))
                         (star-ov (make-overlay line-beg (1+ line-beg))))
                    (overlay-put star-ov 'display "*")
                    (overlay-put star-ov 'my-current-block t)))))
            (forward-line 1)))))))

(add-hook 'org-agenda-finalize-hook #'my-org-agenda-highlight-current-block)

(setq org-agenda-use-time-grid nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-gcal
;;
;; Sync an org file with Google Calendar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-gcal-default-calendar "7d9f481f8f2db790af8a25b1c87036285fc3ea1eaf6b97d4a187e6fccba632a7@group.calendar.google.com")

(defmacro when-not (cond &rest body)
  "Execute BODY when COND is nil (the inverse of `when')."
  `(unless ,cond
     ,@body))

;; Indent it like `unless`
(put 'when-not 'lisp-indent-function '1)

(use-package org-gcal
  :ensure t

  :config
  (setq org-gcal-client-id (encrypted-file-contents "~/.emacs.d/org-gcal/client-id.asc")
	org-gcal-client-secret (encrypted-file-contents "~/.emacs.d/org-gcal/client-secret.asc")
	org-gcal-fetch-file-alist `((,org-gcal-default-calendar .  "~/personal.org")))
  (org-gcal-reload-client-id-secret)
  (setq plstore-cache-passphrase-for-symmetric-encryption t))

(defun my/org-gcal-post-all-scheduled-in-buffer ()
  "Create or update Google Calendar events for all SCHEDULED/DEADLINE
headings in the current buffer using org-gcal."
  (interactive)
  (save-excursion
    (org-map-entries
     (lambda ()
       (when (org-get-scheduled-time (point))
	 (when-not (org-entry-get (point) org-gcal-calendar-id-property)
	   (org-entry-put (point) org-gcal-calendar-id-property org-gcal-default-calendar))
	 (when-not (string= "DONE" (org-get-todo-state))
	   (org-gcal-post-at-point))))
     nil 'file)))

(defun my/debug-org-heading-enumeration ()
  "Sometimes org-map-entries hangs. Use this to figure out where."
  (interactive)
  (delete-file "/tmp/progress.txt")
  (setq my/testing-counter 0)
  (save-excursion
    (org-map-entries
     (lambda ()
       (setq my/testing-counter (1+ my/testing-counter))
       (when (zerop (mod my/testing-counter 100))
	 (message "Counter %d" my/testing-counter))
       (append-to-file (s-concat (org-get-heading) "\n") nil "/tmp/progress.txt")
       (when (org-get-scheduled-time (point))
	 (when-not (string= "DONE" (org-get-todo-state))
	   (message "Found one %s" (org-get-heading)))))
     nil 'file)))

(defun my/org-gcal-post-all-scheduled-in-subtree ()
  "Post all scheduled/deadline entries in the current subtree."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (my/org-gcal-post-all-scheduled-in-buffer)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Load nxhtml-mode (with MuMaMo)
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (load "~/.emacs.d/custom/nxhtml/autostart.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Deletion moves to trash rather than permanently deleting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq trash-directory "~/.Trash")

;; See `trash-directory' as it requires defining `system-move-file-to-trash'.
(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash."
  (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"brew install trash\"")
  (call-process "trash" nil 0 nil "-F"  file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start an emacs server except on Windows,
;; where it would open a TCP socket. But don't
;; start one if one is already running
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (or (eq system-type 'windows-nt)
            (if (fboundp 'server-running-p)
                (server-running-p)
              server-process))
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Do any initialization that's specific to this machine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file-if-exists "~/local-init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the load-path. Generally it is best to set all the load
;; paths here in case there are dependencies between libraries.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-load-paths
      '("~/.emacs.d/custom/candera/"
        "~/.emacs.d/custom/"

        "~/.emacs.d/custom/flexwiki"))

(setq load-path (append custom-load-paths load-path))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Stuff we want to load right away
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Do stuff whenever emacs is idle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar idle-save-buffer-list '())
(defvar idle-save-buffer-time 10)

(defvar idle-save-timer
  (run-with-idle-timer
   idle-save-buffer-time
   t
   (lambda ()
     (dolist (b idle-save-buffer-list)
       (if (buffer-live-p b)
           (when (and (buffer-file-name b)
                      (buffer-modified-p b))
             (save-excursion
               (save-window-excursion
                 (message "Saving %s because emacs is idle." (buffer-name b))
                 (switch-to-buffer b)
                 (save-buffer))))
         ;; Buffer has been killed toss it out of the list
         (setq idle-save-buffer-list (remove b idle-save-buffer-list)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Change the way emacs handles buffer
;; ;; names for files with the same name.
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'uniquify)
;; (setq
;;  uniquify-buffer-name-style 'forward ;; 'post-forward
;;  uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets various miscellaneous settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers
(global-display-line-numbers-mode t)

;; Highlight current line
(global-hl-line-mode 1)

;; ;; Enable CUA-style undo, cut, copy, paste, and selection
;; ;;(cua-mode t)

;; ;; For the love of all that is holy, unify the emacs kill ring and the
;; ;; clipboard
;; (setq x-select-enable-clipboard t)

;; ;; And don't allow tabs to be inserted
;; (setq-default indent-tabs-mode nil)

;; ;; Enable ido-mode
;; (ido-mode 1)

;; ;; And add a space after the line number in text-only terminals
;; ;; (unless window-system (setq linum-format "%d "))

;; (defun scroll-other-window-up-one ()
;;   "Scrolls other window towards top of buffer by one line"
;;   (interactive)
;;   (scroll-other-window-down 1))

;; (defun scroll-other-window-down-one ()
;;   "Scrolls other window towards bottom of buffer by one line"
;;   (interactive)
;;   (scroll-other-window-down -1))

;; (defun scroll-window-up-one ()
;;   "Scrolls other window towards top of buffer by one line"
;;   (interactive)
;;   (scroll-down 1))

;; (defun scroll-window-down-one ()
;;   "Scrolls other window towards bottom of buffer by one line"
;;   (interactive)
;;   (scroll-down -1))

;;; I like this function because I run emacs maximized on a wide
;;; monitor, and it always looks weird to me to edit text way off to
;;; the left edge of the monitor. So I can use this to get something
;;; more visually balanced.
;;;
;;; This function needs a little work to ensure that the other windows
;;; contain interesting stuff, too, like maybe the windows that were
;;; already on the screen.
(defvar former-window-configuration nil
  "Stores previous window configurations, e.g. those that were in effect when center-window-horizontally was called.")

(defun center-window-horizontally (width)
  "Arrange windows three as side-by-side, with the center one
having width WIDTH.
With a numeric prefix arg, specifies width. Otherwise sets
width to 60% frame width, or 85, whichever is larger."
  (interactive "P")
  (push (current-window-configuration) former-window-configuration)
  (let ((width (or width (max 85 (/ (* (frame-parameter nil 'width) 2) 5)))))
    (let ((side-window-width (/ (* (- (frame-parameter nil 'width) width) 10) 19)))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally side-window-width)
                         (other-buffer nil nil))
      (other-window 1)
      (set-window-buffer (split-window-horizontally (- side-window-width))
                         (other-buffer nil nil)))))

(defun temporarily-display-one-window ()
  "Temporarily go to single-window configuration, saving the old
  configuration."
  (interactive)
  (push (current-window-configuration) former-window-configuration)
  (delete-other-windows))

(defun temporarily-display-two-windows (split-vertically?)
  "Temporarily go to two side-by-side windows, saving the old
  configuration. With a prefix argument, the windows are split
  vertically (i.e. one window above the other). By default they
  are split horizontally."
  (interactive "P")
  (push (current-window-configuration) former-window-configuration)
  (delete-other-windows)
  (if split-vertically?
      (split-window-vertically)
    (split-window-horizontally)))

(defun temporarily-display-three-windows (large-window-horizontal?)
  "Temporarily go to a three-window configuration, with one large
  window and two small ones, saving the old configuration. With a
  prefix argument, the largest window is fills the frame
  horizontally. Otherwise it fills the frame vertically."
  (interactive "P")
  (push (current-window-configuration) former-window-configuration)
  (delete-other-windows)
  (if large-window-horizontal?
      (split-window-vertically)
    (split-window-horizontally))
  (other-window 1)
  (if large-window-horizontal?
      (split-window-horizontally)
    (split-window-vertically)))

(defun temporarily-display-four-windows ()
  "Temporarily go to four equally-sized windows, saving the old
  configuration."
  (interactive)
  (push (current-window-configuration) former-window-configuration)
  (delete-other-windows)
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally))

(defun restore-former-window-configuration ()
  "Restore the window configuration that was in effect before
  `center-window-horizontally' was called."
  (interactive)
  (let ((former (pop former-window-configuration)))
    (if former
        (set-window-configuration former)
      (message "No previous window configuration"))))

(defun candera-next-window ()
  "Move to the next window."
  (interactive)
  (other-window 1))

(defun candera-previous-window ()
  "Move to the previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x 4 C-c") 'center-window-horizontally)
(global-set-key (kbd "C-x 4 C-r") 'restore-former-window-configuration)
(global-set-key (kbd "C-x 4 1") 'temporarily-display-one-window)
(global-set-key (kbd "C-x 4 2") 'temporarily-display-two-windows)
(global-set-key (kbd "C-x 4 3") 'temporarily-display-three-windows)
(global-set-key (kbd "C-x 4 4") 'temporarily-display-four-windows)
(global-set-key (kbd "C-x 5 n") 'select-frame-by-name)
(global-set-key (kbd "M-N") 'candera-next-window)
(global-set-key (kbd "M-P") 'candera-previous-window)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "C-c d") 'sdcv-search)
(global-set-key (kbd "C-c D") 'define-word-at-point)
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-w"))
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-N"))
(global-unset-key (kbd "s-X"))
(global-unset-key (kbd "s-e"))

;; Auto-complete customization. Might want to make this mode-specific
;; (global-set-key (kbd "M-/") 'auto-complete)
;; (require 'auto-complete)
;; (define-key ac-completing-map (kbd "C-n") 'ac-next)
;; (define-key ac-completing-map (kbd "C-p") 'ac-previous)

;; ;; Aliases for super-keys I confuse with the meta equivalent.
;; (global-set-key (kbd "s-N") 'other-window)
;; (global-set-key (kbd "s-y") 'yank-pop)
;; (global-set-key (kbd "s-b") 'backward-word)

;; Flips the left and right windows. Taken from
;; http://whattheemacsd.com//buffer-defuns.el-02.html
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
	 (while  (< i numWindows)
           (let* ((w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))


;; Cursor-style setting functions
(defun set-cursor-type (cursor)
  "Modify the cursor to the specified type"
  (interactive "sCursor type (bar, box, etc.): ")
  (modify-frame-parameters
   (selected-frame)
   (list (cons 'cursor-type (intern cursor)))))

(defun set-bar-cursor ()
  "Change the cursor to a bar rather than the (default) box"
  (interactive)
  (set-cursor-type "bar"))

(defun set-box-cursor ()
  "Change the cursor to a box (the default style)"
  (interactive)
  (set-cursor-type "box"))

(defun json-unescape-region ()
  "Replaces JSON escaped characters like newlines and quotes with their literal equivalents in the region."
  (interactive)
  (save-excursion 
    (replace-string-in-region "\\n" "\n" (region-beginning) (region-end))
    (replace-string-in-region "\\\"" "\"" (region-beginning) (region-end))))

(defun set-random-background-color ()
  "Set a random darkish background color for the current frame"
  (interactive)
  (set-background-color (format "#%02x%02x%02x" (random 60) (random 60) (random 60))))


(defun my/do-something (&optional user-str)
  "Do something. With a prefix arg (C-u), prompt for USER-STR; without, don't."
  (interactive
   ;; Only prompt if a prefix argument was supplied.
   (if current-prefix-arg
       (list (read-string "String: "))
     (list nil)))
  ;; Example behavior: just report what we got (or that we didn't prompt).
  (if user-str
      (message "You entered: %s" user-str)
    (message "No prompt; running with defaults.")))


(defun candera-new-frame (&optional frame-name)
  "Makes a new frame, sets a random background color, and configures it the
way Craig likes it. If called with a prefix argument, does not prompts
for a frame name."
  (interactive
   (if (not current-prefix-arg)
       (list (read-string "Frame name: "))
     (list nil)))
  (when (and frame-name
	     (cl-some (lambda (n) (string= n frame-name))
		      (cl-map 'list 'car (make-frame-names-alist))))
    (error "Frame with name %s already exists" frame-name))
  (let ((frame (make-frame)))
    (set-random-background-color)
    (set-bar-cursor)
    (set-frame-parameter frame 'fullscreen 'maximized)
    (when frame-name
      (set-frame-name frame-name))))

(global-set-key (kbd "C-x 5 C") 'candera-new-frame)

(defun google-word-at-point ()
  "Opens a browser for the word at point on google.co"
  (interactive)
  (browse-url (format "https://www.google.com/search?q=%s" (word-at-point))))

(set-bar-cursor)

;; A single space ends a sentence
(setq sentence-end-double-space nil)

;; ;; Use forward slashes between directory elements
;; ;; TODO: This symbol has been deprecated. Change if there's a problem
;; (setq directory-sep-char ?\/)

;; ;; Don't blink the cursor
;; (setq blink-cursor nil)

;; ;; Move the mouse pointer out of the way when the cursor is near it
;; (mouse-avoidance-mode 'cat-and-mouse)

;; Turn off the menu bar and the tool bar, since I never use them.
;; Except under OS X, where they don't take up space
(unless (eq system-type 'darwin) (menu-bar-mode -1))

;; On some machines, tool-bar-mode is not bound, and it causes
;; initialization to bomb.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; ;; Set up some keybindings that I like
;; (global-set-key (quote [C-M-down]) 'scroll-other-window-down-one)
;; (global-set-key (quote [C-M-up])   'scroll-other-window-up-one)
;; (global-set-key (quote [C-up]) 'scroll-window-up-one)
;; (global-set-key (quote [C-down]) 'scroll-window-down-one)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (global-set-key (quote [C-tab]) 'other-window)

;; (global-set-key [f12] 'eshell)
;; ;;(global-set-key [f11] (lambda () (interactive) (switch-to-buffer "*nrepl*")))
;; (global-set-key [f4] 'call-last-kbd-macro)
;; (global-set-key [f8] 'next-error)
;; (column-number-mode ())

;; (setq default-major-mode 'text-mode)
;; (setq display-time-day-and-date 'true)
;; ;; display-time has the wonderful effect of causing my emacs to
;; ;; totally hang for the first five seconds of every minute on windows.
;; ;;(display-time)

;; text-mode-hook runs even in modes derived from text-mode, like
;; javascript-mode, where having flyspell turned on is not exactly
;; handy. Turn it off in modes where we don't want it.
(defun turn-on-flyspell ()
  "Turns on flyspell-mode"
  (interactive)
  (flyspell-mode 1))

(defun turn-off-flyspell ()
  "Turns off flyspell-mode"
  (interactive)
  (flyspell-mode 0))

(add-hook 'emacs-lisp-mode-hook 'turn-off-flyspell)
(add-hook 'ruby-mode-hook 'turn-off-flyspell)

(defun inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (let ((result (nth 4 (syntax-ppss))))
    (message "%s" result)
    result))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Calendar customization
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; 38.8526° N, 77.3044° W
;; (setq calendar-latitude 38.9)
;; (setq calendar-longitude -77.3)
;; (setq calendar-location-name "Fairfax, VA")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Whitespace cleanup and display
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Friends don't let friends save source code with tabs in it
(defun detabify-buffer ()
  "Calls untabify on the current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defvar detabify-modes '(javascript-mode emacs-lisp-mode ruby-mode clojure-mode java-mode)
  "A list of the modes that will have tabs converted to spaces before saving.")

(defun mode-aware-detabify ()
  "Calls untabify on the current buffer if the major mode is one of 'detabify-modes'"
  (interactive)
  (when (member major-mode detabify-modes)
    (detabify-buffer)))

(defvar delete-trailing-whitespace-modes detabify-modes
  "A list of the modes that will have trailing whitespace before saving.")

(defun mode-aware-trailing-whitespace-cleanup ()
  "Calls delete-trailing-whitespace-modes on the current buffer
if the major mode is one of 'delete-trailing-whitespace-modes'"
  (interactive)
  (when (member major-mode delete-trailing-whitespace-modes)
    (delete-trailing-whitespace)))

;; Removing these in favor of doing it manually
; (add-hook 'before-save-hook 'mode-aware-detabify)
; (add-hook 'before-save-hook 'mode-aware-trailing-whitespace-cleanup)

(defun clean-up-whitespace ()
  "Calls untabify and delete-trailing-whitespace on the current buffer."
  (interactive)
  (detabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-x t") 'clean-up-whitespace)

;; (defface extra-whitespace-face
;;    '((t (:background "pale green")))
;;    "Used for tabs and such.")

;; (defvar extra-whitespace-keywords
;;   '(("\t" . 'extra-whitespace-face)))

;; (defun setup-highlight-whitespace ()
;;   (font-lock-add-keywords nil extra-whitespace-keywords)
;;   (setq show-trailing-whitespace t))

;; (defun stop-highlighting-whitespace ()
;;   (interactive)
;;   (font-lock-remove-keywords nil extra-whitespace-keywords)
;;   (setq show-trailing-whitespace nil))

;; (add-hook 'emacs-lisp-mode-hook 'setup-highlight-whitespace)
;; (add-hook 'text-mode-hook 'setup-highlight-whitespace)
;; (add-hook 'lisp-mode-hook 'setup-highlight-whitespace)
;; (add-hook 'ruby-mode 'setup-highlight-whitespace)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set M-. to do something useful
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ctags command: ctags -R -e . --language-force=Clojure

;; (global-set-key (kbd "M-.") 'projectile-find-tag)
;; ;; (global-set-key (kbd "M-.") 'xref-find-definitions)
;; ;;(global-set-key (kbd "M-.") 'imenu)
;; ;; (global-set-key (kbd "M-,") 'dumb-jump-back)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set M-' to do completion
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "M-'") 'completion-at-point)

;; ;; (add-hook 'slime-mode-hook
;; ;;           (lambda ()
;; ;;             (define-key slime-mode-map (kbd "M-'") 'slime-complete-symbol)
;; ;;             (define-key slime-mode-map (kbd "C-c M-q") nil)))

;; ;; (add-hook 'slime-repl-mode-hook
;; ;;           (lambda ()
;; ;;             (define-key slime-repl-mode-map (kbd "M-'") 'slime-complete-symbol)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up emacs-lisp-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-paredit-mode ()
  (interactive)
  (paredit-mode 1))

(unless (string= "raspberrypi" system-name)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-paredit-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Work around a bug in Ubuntu 10.10
;; (setq flyspell-issue-welcome-flag nil)

;; ;; Turn on font-lock by default in all modes
;; (global-font-lock-mode t)

;; ;; Highlight region between point and mark
;; (transient-mark-mode t)

;; (defun insert-new-line-above ()
;;   "Insert a new, blank line above the point"
;;   (interactive)
;;   (progn
;;     (move-to-column 0)
;;     (newline)
;;     (previous-line 1)
;;     (indent-according-to-mode)))

;; (global-set-key [\C-return] 'insert-new-line-above)

;; (defun set-big-font ()
;;   "sets the font to something readable from more than 3 inches away"
;;   (interactive)
;;   (modify-frame-parameters
;;    nil
;;    '( (font . "-outline-Courier-bold-r-normal-normal-19-142-96-96-c-110-iso10646-1"))))

;; (defun set-small-font ()
;;   "sets the font to something readable from more than 3 inches away"
;;   (interactive)
;;   (modify-frame-parameters
;;    nil
;;    '( (font . "-outline-Courier-bold-r-normal-normal-12-142-96-96-c-110-iso10646-1"))))

(defun set-default-font-size (height)
  "Sets the default font size to `height`."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height height))

;; (defun set-default-frame-properties ()
;;   "Sets the frame properties back to the defaults"
;;   (interactive)
;;   (modify-frame-parameters
;;    nil
;;    default-frame-alist))

;; (defun insert-current-time ()
;;   "Inserts the current time at point"
;;   (interactive)
;;   (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

;; (global-set-key "\C-c\C-t" 'insert-current-time)

;; ;; Turn on parathesis highlighting in .el files
;; ;(add-hook 'emacs-lisp-mode-hook
;; ;         (lambda () (highlight-parentheses-mode 1)))


;; ;; Tell emacs to wrap lines in vertically split windows
;; ;; Much as I would like to leave this set to nil, it seems to cause all sorts of problems.
;; ;;(setq truncate-partial-width-windows nil)
;; (setq truncate-lines t)

;; ;; Except in inferior-lisp, where it screws things up
;; ;; (add-hook 'inferior-lisp-mode-hook
;; ;;           (lambda ()
;; ;;              (setq truncate-lines t)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up ido
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; Make ido-mode list things vertically
;; ;; (setq ido-decorations
;; ;;       (quote
;; ;;        ("\n-> "           ; Opening bracket around prospect list
;; ;;         ""                ; Closing bracket around prospect list
;; ;;         "\n   "           ; separator between prospects
;; ;;         "\n   ..."        ; appears at end of truncated list of prospects
;; ;;         "["               ; opening bracket around common match string
;; ;;         "]"               ; closing bracket around common match string
;; ;;         " [No match]"     ; displayed when there is no match
;; ;;         " [Matched]"      ; displayed if there is a single match
;; ;;         " [Not readable]" ; current diretory is not readable
;; ;;         " [Too big]"      ; directory too big
;; ;;         " [Confirm]")))   ; confirm creation of new file or buffer

;; ;; ;; And let us use standard navagation keys that make sense vertically
;; ;; (add-hook 'ido-setup-hook
;; ;;           (lambda ()
;; ;;             (define-key ido-completion-map [down] 'ido-next-match)
;; ;;             (define-key ido-completion-map [up] 'ido-prev-match)
;; ;;             (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;; ;;             (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;; ;; ;; Make ido stop prompting me about whether I want to create a new buffer
;; ;; (setq ido-create-new-buffer 'always)

;; (use-package ido-vertical-mode
;;   :ensure t)

;; (require 'ido-vertical-mode)
;; (ido-mode 1)
;; (ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up dired-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dired-insert-this-directory-recursively (include-hidden)
  "Recursively insert the subdirectories of the current dired directory."
  (interactive "P")
  (dired-insert-subdir dired-directory (if (null include-hidden)
                                           "-lR"
                                         "-alR")))

(defun dired-recursive-copy-marked-files-async (target-dir)
  "Recursively moves marked files."
  (interactive "DTarget directory: ")
  (dired-do-shell-command (format "gcp ? --parents '%s'" target-dir)))

;; (defun mrc-dired-do-command (command)
;;   "Run COMMAND on marked files. Any files not already open will be opened.
;; After this command has been run, any buffers it's modified will remain
;; open and unsaved."
;;   (interactive "CRun on marked files M-x ")
;;   (save-window-excursion
;;     (mapc (lambda (filename)
;;             ;; (find-file filename)
;;             (call-interactively command))
;;           (dired-get-marked-files))))

;; (defun emms-dired-play-file ()
;;   "Plays the file at point using emms."
;;   (interactive)
;;   (emms-play-file (dired-get-filename)))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "I") 'dired-insert-this-directory-recursively)
	    (define-key dired-mode-map (kbd "C-c C-c") 'wdired-change-to-wdired-mode)
            ;; (define-key dired-mode-map (kbd "M-p") 'emms-dired-play-file)
	    ))

(setq dired-dwim-target t)

(define-key dired-mode-map (kbd "O") 'dired-display-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; javascript-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'javascript-mode-hook 'turn-off-flyspell)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Associate ruby mode with ruby files
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'auto-mode-alist '("Rakefile$\\|Gemfile$\\|\\.rake$\\|Capfile$\\|\\.watchr$\\|Guardfile$\\|\\.ru$\\|\\.gemspec$" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Associate yaml-mode with yml files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set M-. to be imenu where SLIME isn't available
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (add-hook 'ruby-mode-hook (lambda () (define-key ruby-mode-map (kbd "M-.") 'imenu)))
;; ;; (define-key emacs-lisp-mode-map (kbd "M-.") 'imenu)
;; ;; (add-hook 'javascript-mode-hook (lambda () (define-key js-mode-map (kbd "M-.") 'imenu)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; This section sets up simple refactoring support
;; ;; (only symbol renaming at this point)
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;(require 'refactor)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up lisp-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Support paredit for better paren
;; ;;; editing in Lisp mode
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode-hook . (lambda () (paredit-mode +1)))
  :hook (lisp-mode-hook . (lambda () (paredit-mode +1)))
  :hook (emacs-lisp-mode-hook . (lambda () (paredit-mode +1)))
  :hook (clojure-mode-hook . (lambda () (paredit-mode +1)))
  :hook (javascript-mode-hook . (lambda () (paredit-mode +1)))
  :config
  (show-paren-mode t))

(define-key paredit-mode-map (kbd "M-)")
  'paredit-close-parenthesis-and-newline)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Support for SLIME
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (add-to-list 'load-path "~/.emacs.d/custom/slime-2010-05-19/")  ; your SLIME directory
;; ;; (add-to-list 'load-path "~/.emacs.d/custom/slime-2010-05-19/contrib")  ; your SLIME contrib directory


;; ;; ;;(add-to-list 'load-path "~/emacs/slime-2008-11-03/")  ; your SLIME directory
;; ;; ;;(add-to-list 'load-path "~/emacs/slime-2008-11-03/contrib")  ; your SLIME contrib directory
;; ;; ;; (setq
;; ;; ;;   ; inferior-lisp-program "C:/bin/clisp-2.45/clisp -K full"  ; your Lisp system
;; ;; ;;   ;inferior-lisp-program "C:/bin/sbcl-1.0.14.22/sbcl --core C:/bin/sbcl-1.0.14.22/sbcl.core"  ; your Lisp system
;; ;; ;;   slime-complete-symbol-function 'slime-fuzzy-complete-symbol  ; fuzzy symbol completion (requires slime-fuzzy from contrib)
;; ;; ;;   ;slime-complete-symbol-function 'slime-complete-symbol  ; standard symbol completion
;; ;; ;;   lisp-indent-function 'common-lisp-indent-function            ; How would you like to indent?
;; ;; ;;  )

;; ;; (require 'slime-autoloads)
;; ;; (slime-setup '(slime-repl slime-editing-commands slime-fuzzy slime-presentations slime-scratch))
;; ;; ;(slime-setup '(slime-fancy))

;; ;; ;(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)  ; fuzzy symbol completion (requires slime-fuzzy from contrib)

;; ;; (if (functionp 'slime-local-setup)
;; ;;     (slime-local-setup))

;; ;; ;; Turn off the annoying SLIME version mismatch message

;; ;; (eval-after-load 'slime '(setq slime-protocol-version 'ignore))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Autocomplete for SLIME
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'ac-slime)
;; ;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; ;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Allow input to be sent to somewhere other than inferior-lisp
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun shell-send-input (input &optional buf)
;;   "Send INPUT into the *shell* buffer (or `buf` if specified) and leave it visible."
;;   (save-selected-window
;;     (switch-to-buffer-other-window (or buf "*shell*"))
;;     (goto-char (point-max))
;;     (insert input)
;;     (comint-send-input)))

;; (defun defun-at-point ()
;;   "Return the text of the defun at point."
;;   (apply #'buffer-substring-no-properties
;;          (region-for-defun-at-point)))

;; (defun region-for-defun-at-point ()
;;   "Return the start and end position of defun at point."
;;   (save-excursion
;;     (save-match-data
;;       (end-of-defun)
;;       (let ((end (point)))
;;         (beginning-of-defun)
;;         (list (point) end)))))

;; (defun expression-preceding-point ()
;;   "Return the expression preceding point as a string."
;;   (buffer-substring-no-properties
;;    (save-excursion (backward-sexp) (point))
;;    (point)))

;; (defun shell-eval-last-expression ()
;;   "Send the expression preceding point to the *shell* buffer."
;;   (interactive)
;;   (shell-send-input (expression-preceding-point)))

;; (defun shell-eval-defun ()
;;   "Send the current toplevel expression to the *shell* buffer."
;;   (interactive)
;;   (shell-send-input (defun-at-point)))

;; (defun shell-eval-region ()
;;   "Send the contents of the region to the *shell* buffer."
;;   (interactive)
;;   (if (use-region-p)
;;     (shell-send-input (buffer-substring-no-properties
;;                        (region-beginning)
;;                        (region-end)))
;;     (error "The region is not active - nothing to evaluate")))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up org-babel
;; ;; Stolen from https://github.com/stuartsierra/dotfiles/blob/2ec5ab2a45c091d74c8e73d62683b15ddd8bd9c7/.emacs.d/local/init.el#L295
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'org)
;; (require 'ob)
;; (require 'ob-tangle)
;; (require 'ob-clojure)
;; ;;(setq org-babel-clojure-backend 'cider)
;; ;;(require 'cider)

;; ;; Don't make me confirm evaluation every single time
;; (setq org-confirm-babel-evaluate nil)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (clojure . t)
;;    (shell . t)
;;    (dot . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-trello
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tried to set it up 2023-Jul, not working. Probably victim of Atlassian crapware issue.

;; (use-package org-trello
;;   :ensure t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; This section sets up clojure-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Require clojure-mode to load and associate it to all .clj files.

(use-package clojure-mode
  :ensure t

  :bind
  (:map clojure-mode-map
	("C-x C-e" . inf-clojure-eval-last-sexp)))

;; (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.hl$" . clojurescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljc$" . clojurec-mode))
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

;; ;; (require 'dumb-jump)
;; ;; (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljc"))
;; ;; (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs"))
;; ;; (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs.hl"))

;; ;; Turn on paredit for clojure files
;; ;(require 'clojure-paredit)
;; (setq clojure-enable-paredit t)

;; ;; This gives us a way to turn off slime-mode via .dir-locals.el. Just
;; ;; execute add-dir-local-variable to set clojure-mode-no-slime to t,
;; ;; and after that slime-mode will be turned off in any clojure-mode
;; ;; buffer that gets opened in that directory structure.
;; (defvar clojure-mode-no-slime nil)

;; ;; We have to use hack-local-variables-hook, because apparently
;; ;; clojure-mode-hook runs before the local variables are set.
;; (add-hook 'hack-local-variables-hook
;;           '(lambda () (when clojure-mode-no-slime
;;                         (message "Disabling slime-mode because clojure-mode-no-slime is set")
;;                         (slime-mode -1))))

;; ;; Work around bug where inf-clojure-minor-mode sets
;; ;; comint-input-sender too aggressively
;; (make-variable-buffer-local comint-input-sender)

;; These are extra key defines because I kept typing them.
;; Within clojure-mode, have Ctrl-x Ctrl-e evaluate the last
;; expression.
;; Ctrl-c Ctrl-e is also there, because I kept typoing it.
(defun setup-clojure-mode ()
  ;; Buffer local variables are not visible in mode hooks
  (add-hook 'hack-local-variables-hook
            (lambda ()
              (when use-inf-clojure
                (inf-clojure-minor-mode 1)
                ;; (eldoc-mode 1)
                ;; (cider-mode 0)
                ))
            nil
            t)
  ;; (highlight-parentheses-mode 1)
  ;; (unless (eq major-mode 'cider-repl-mode)
  ;;   (display-line-numbers-mode 1))
  (highlight-symbol-mode t)
  (paredit-mode 1)
  (hs-minor-mode 1)
  (setq show-trailing-whitespace t)
  (flyspell-mode 0)
  (when (and (not use-inf-clojure)
             (fboundp 'clojure-enable-nrepl))
    (clojure-enable-nrepl))
  (define-key clojure-mode-map (kbd "C-c e") 'shell-eval-last-expression)
  (define-key clojure-mode-map (kbd "C-c x") 'shell-eval-defun)
  (define-key clojure-mode-map (kbd "C-c C-e") 'lisp-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
  ;; Fix the keys that paredit screws up
  (define-key paredit-mode-map (kbd "<C-left>") nil)
  (define-key paredit-mode-map (kbd "<C-right>") nil)
  ;; And define some new bindings since the OS eats some of the useful ones
  (define-key paredit-mode-map (kbd "<C-S-left>") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "<C-S-right>") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "<M-S-left>") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "<M-S-right>") 'paredit-forward-barf-sexp)
  ;; Not all terminals can transmit the standard key sequencences for
  ;; paredit-forward-slurp-sexp, which is super-useful
  (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

(add-hook 'clojure-mode-hook #'setup-clojure-mode)

;; Not really specific to clojure-mode: make comments fill as if they
;; were multi-line, which they often are
(setq comment-multi-line t)

;; For core.match
;; (put-clojure-indent 'match 1) ; Like let

;; ;; clojure-fill-docstring got changed rather radically in a newer
;; ;; version of clojure-mode than the one I use. I prefer the one I
;; ;; wrote, so I override it here. I also made a few changes, like
;; ;; respecting markdown syntax to give me things like correctly
;; ;; indenting bulleted lists.
;; (defun clojure-fill-docstring ()
;;   "Fill the definition that the point is on appropriate for Clojure.

;; Fills so that every paragraph has a minimum of two initial spaces,
;; with the exception of the first line.  Fill margins are taken from
;; paragraph start, so a paragraph that begins with four spaces will
;; remain indented by four spaces after refilling."
;;   (interactive)
;;   (if (and (fboundp 'paredit-in-string-p) paredit-mode)
;;       (unless (paredit-in-string-p)
;;         (error "Must be inside a string")))
;;   ;; Oddly, save-excursion doesn't do a good job of preserving point.
;;   ;; It's probably because we delete the string and then re-insert it.
;;   (let ((old-point (point)))
;;     (save-restriction
;;       (save-excursion
;;         (let* ((clojure-fill-column 70)
;;                (string-region (clojure-docstring-start+end-points))
;;                (string-start (1+ (car string-region)))
;;                (string-end (1- (cdr string-region)))
;;                (string (buffer-substring-no-properties string-start
;;                                                        string-end)))
;;           (delete-region string-start string-end)
;;           (insert
;;            (with-temp-buffer
;;              ;; Bah, this doesn't work, because it isn't idempotent.
;;              ;; To make it so, and to preserve correctly line flow for
;;              ;; things like bulleted lists, it looks like we might
;;              ;; have to heuristically detect that every non-blank line
;;              ;; starts with two spaces and remove them before trying
;;              ;; again. I think the fix might be to make
;;              ;; `markdown-adaptive-fill-function` aware of
;;              ;; `left-margin`.
;;              (insert string)
;;              (markdown-mode)
;;              (setq fill-column (- clojure-fill-column 2))
;;              (fill-region (point-min) (point-max))
;;              (goto-char (point-min))
;;              (replace-regexp "^" "  ")
;;              (delete-trailing-whitespace)
;;              (buffer-substring-no-properties (+ 2 (point-min)) (point-max)))))))
;;     (goto-char old-point)))

(defun find-clojure-namespaced-tag ()
  "Find the tag to search for at point. Remove the namespace
  prefix, since xref doesn't do a good job of dealing with it."
  (let ((tag (find-tag-default)))
    (when tag
      (string-match "\\([^/]*/\\)?\\([^/]+\\)" tag)
      (match-string 2 tag))))

(put 'clojure-mode 'find-tag-default-function #'find-clojure-namespaced-tag)
(put 'clojurescript-mode 'find-tag-default-function #'find-clojure-namespaced-tag)
(put 'clojurec-mode 'find-tag-default-function #'find-clojure-namespaced-tag)

;; Don't ask before reading the updated TAGS file
(setq tags-revert-without-query t)

(defun clojure-edit-string-in-other-buffer ()
  "Opens a temporary buffer and populates it with the contents of
the string at point. Hitting C-c C-c in that buffer will save it
back to the original string."
  (interactive)
  (save-mark-and-excursion
    (paredit-backward-up)
    (lexical-let* ((orig (current-buffer))
                   (start (point))
                   (_ (paredit-forward))
                   (end (point))
                   (contents (buffer-substring-no-properties (1+ start) (1- end)))
		   (finish (lambda ()
                             (interactive)
                             (save-mark-and-excursion
                               (lexical-let* ((contents (buffer-substring-no-properties
                                                         (point-min)
                                                         (point-max))))
                                 (switch-to-buffer orig)
                                 (kill-region (1+ start) (1- end))
                                 (goto-char (1+ start))
                                 (insert contents))))))
      (lexical-let* ((new-buffer (switch-to-buffer (make-temp-name "clojure-string")))))
      (insert contents)
      (save-mark-and-excursion
        (mark-whole-buffer)
        (replace-string "\\n" "\n")
        (mark-whole-buffer)
	(replace-string "\\\"" "\""))
      (normal-mode)
      (local-set-key (kbd "C-c C-c") finish)
      (local-set-key (kbd "C-c C-f") finish))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; inferior-lisp
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'inf-lisp)
;; (require 'clojure-mode)
;; (require 'paredit)

;; ;; Right now I only use inferior lisp for clojure, so we configure for that
;; (define-key clojure-mode-map (kbd "C-c C-c") 'lisp-eval-defun)

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (paredit-mode t)))

;; (defun comint-clear-buffer ()
;;   (interactive)
;;   (let ((comint-buffer-maximum-size 0))
;;     (comint-truncate-buffer)))

;; (define-key inferior-lisp-mode-map (kbd "C-c M-o") #'comint-clear-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cider
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package cider
;;   :ensure t)

;; ;; ;; Don't use on the Pi, due to excessive CPU
;; ;; (unless (string= "raspberrypi" system-name)
;; ;;   (add-hook 'cider-mode-hook
;; ;;             (lambda ()
;; ;;               (eldoc-mode)
;; ;;               (cider-eldoc-setup)
;; ;;               ;; Suppress some really stupid shit that cider is doing
;; ;;               ;; around background colors. I think it's assuming
;; ;;               ;; there's a theme. Or it could be the problem that when
;; ;;               ;; cider loads, the background color is still light.
;; ;;               ;; (setq cider-stacktrace-frames-background-color
;; ;;               ;;       (cider-scale-background-color))
;; ;;               )))

;; ;; (add-hook 'cider-repl-mode-hook
;; ;;           (lambda ()
;; ;;             (paredit-mode 1)
;; ;;             ;; For some reason this isn't defined correctly
;; ;;             (define-key cider-repl-mode-map (kbd "{") #'paredit-open-curly)
;; ;;             (define-key cider-repl-mode-map (kbd "}") #'paredit-close-curly)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; elein-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'elein)

;; ;; (defun lein-repl ()
;; ;;   "Run 'lein repl' in an inferior-lisp."
;; ;;   (interactive)
;; ;;   (inferior-lisp "lein repl"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; This section sets up Craig's web-lookup utilities
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-lookup)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; This section sets up Craig's typing-speed-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load "~/.emacs.d/custom/candera/typing-speed.el")
;; ;; (add-hook 'text-mode-hook (lambda ()
;; ;;                             (turn-on-typing-speed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up Craig's view-visited-file-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'view-visited-file)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; This section sets up Craig's outline-presentation-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on Craig's outline-presentation hacks.
(load "~/.emacs.d/custom/candera/outline-presentation.el")

;; (global-set-key (quote [f5]) 'outline-presentation-mode-on)
;; (global-set-key (quote [f6]) 'lisp-mode)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; This section sets up Craig's journal functionality
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/custom/candera/journal.el")
(global-set-key (kbd "C-x y") 'find-yesterday-log-file)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; sgml-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sgml-mode)

;; (setq auto-mode-alist
;;       (append '(
;;              ("\\.xml$" . sgml-mode)
;;              ("\\.build$" . sgml-mode)
;;              ("\\.config$" . sgml-mode)
;;              ("\\.xslt$" . sgml-mode)
;;              ("\\.xsl$" . sgml-mode)
;;              ("\\.xsd$" . sgml-mode)
;;              ) auto-mode-alist ))

;; (defun sgml-pretty-print-buffer ()
;;   "Format the entire buffer using sgml-pretty-print"
;;   (interactive)
;;   (save-excursion
;;     (sgml-pretty-print (point-min) (point-max))))

;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (define-key sgml-mode-map (kbd "C-c p") 'sgml-pretty-print-buffer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;
;; ; Set up the command shell
;; ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-shell-setup ()
  "For cmdproxy shell under Emacs 20"
  (setq w32-quote-process-args ?\")
  (make-variable-buffer-local 'comint-completion-addsuffix))

(add-hook 'shell-mode-hook 'my-shell-setup)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;
;; ; This sets CVS to use plink, which SourceForge requires
;; ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setenv "CVS_RSH" "plink")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;
;; ; This sets up my FlexWiki mode
;; ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.wiki$" . flexwiki-mode))
;; (require 'flexwiki)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;
;; ; Sets up outline-mode
;; ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.outline$" . outline-mode))
;; (add-hook 'outline-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sets up highlight-symbol mode
;;
;; http://nschum.de/src/emacs/highlight-symbol/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package highlight-symbol
  :ensure t)

;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up markdown-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package markdown-mode
;;   :ensure t
;;   :mode "\\.md"
;;   :config
;;   (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up magit and magit-modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  ;; Broken in the 20230107.2134 release
  ;; :straight (:host github :repo "magit/magit" :commit "161ab485209ecd0f304e16ca95f8a145327e7ffe")
  :config
  ;; For some weird reason, when this is true, delete does nothing. I
  ;; even debugged it and couldnt' figure out why.
  (setq magit-delete-by-moving-to-trash nil)
  )

(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/custom/magit/Documentation/"))

;; C-x m is normally compose-mail, but I never do mail inside of
;; emacs, whereas I run magit-status all the time
(global-set-key (kbd "C-x m") 'magit-status)

(define-key magit-mode-map (kbd "^") 'magit-goto-parent-section)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

;; Work around stupid bug where magit launches a second Emacs.
;; See https://github.com/magit/magit/issues/862
(when (eq 'darwin system-type)
  (set-variable 'magit-emacsclient-executable
                "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up gist.el
;; ;;
;; ;; http://github.com/defunkt/gist.el
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'gist)

;; ;; ;; Turn on longlines mode whenever we're in an edit server buffer
;; ;; (add-hook 'edit-server-text-mode-hook
;; ;;           '(lambda ()
;; ;;              (longlines-mode 1)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; diary setup
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq diary-file "~/Dropbox/diary")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up nXhtml mode
;; ;;
;; ;; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;(load "~/.emacs.d/custom/nxhtml/autostart.el")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Change zap-to-char not to kill the char
;; ;; it zaps to. Taken from
;; ;; http://www.emacswiki.org/emacs/ZapUpToChar
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
;;   "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
;;   The CHAR is replaced and the point is put before CHAR."
;;   (insert char)
;;   (forward-char -1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Interactively evaluate SPARQL
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (autoload 'sparql-mode "sparql-mode.el"
;;      "Major mode for editing SPARQL files" t)
;; (add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

;; (setq sparql-default-base-url "http://localhost:2020/metamodl_test/")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Set up ERC
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;(add-hook 'erc-insert-post-hook 'ding)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; find-file-in-git-repo
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Deprecating in favor of find-file-in-project
;; ;; (require 'find-file-in-git-repo)
;; ;; (global-set-key (kbd "C-x M-f") 'find-file-in-git-repo)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; find-file-in-project
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'find-file-in-project)
;; ;; (global-set-key (kbd "C-x M-f") 'find-file-in-project)
;; ;; (setq ffip-patterns (append '("*.clj" "*.cljc" "*.cljs" "*.scss" ".css" "*.java" "*.dtm" "*.edn") ffip-patterns))
;; ;; (setq ffip-limit 2048)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; helm-projectile
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (use-package helm-projectile
;; ;;   :ensure t)

;; ;; (require 'helm-projectile)
;; ;; (global-set-key (kbd "C-x M-f") 'helm-projectile-find-file)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; helm-ag
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (use-package helm-ag
;; ;;   :ensure t)

;; ;; ;; Even though it breaks sometimes, it's still better than counsel-ag
;; ;; ;; because it uses a separate buffer.
;; ;; (require 'helm-ag)
;; ;; (global-set-key (kbd "C-x M-s") 'helm-do-ag-project-root)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; helm-org
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package helm-org
;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; align-cljlet, for aligning let forms
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'align-cljlet)
;; ;; (define-key clojure-mode-map (kbd "C-c |") 'align-cljlet)

;; ;; Deprecated - replaced by clojure-mode support
;; (define-key clojure-mode-map (kbd "C-c |") 'clojure-align)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SMEX
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  ;; (global-set-key (kbd "M-x") 'smex)
  ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy, Counsel, Company
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun candera-ivy-sort-by-length (x y)
  "Sort strings by length"
  ;; (message "candera-ivy-sort-by-length %s %s" x y)
  (< (length x) (length y)))

(defun candera-ivy-heuristic-sort (name candidates)
  "Use my heuristics to sort for a good match."
  ;; (message "heuristics sorting %s" name)
  candidates)

(use-package ivy
  :ensure t
  :config
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus)))
  ;; ivy-sort-functions-alist controls the initial sort;
  ;; (add-to-list 'ivy-sort-functions-alist
  ;; 	       '(ivy-switch-buffer    . candera-ivy-sort-by-length))
  ;; (add-to-list 'ivy-sort-functions-alist
  ;; 	       '(counsel-find-file    . candera-ivy-sort-by-length))
  ;; (add-to-list 'ivy-sort-functions-alist
  ;; 	       '(projectile-find-file . candera-ivy-sort-by-length))
  ;; ivy-sort-matches-functions-alist are called after each input to
  ;; sort what remains.
  ;; (add-to-list 'ivy-sort-matches-functions-alist
  ;; 	       '(ivy-switch-buffer . candera-ivy-heuristic-sort))
  ;; (global-set-key (kbd "C-s") 'swiper-isearch)

  :bind
  (:map ivy-minibuffer-map
	("RET" . 'ivy-alt-done)))

;; Better sorting for ivy--regex-fuzzy
(use-package flx
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "C-y") 'counsel-yank-pop)
  )

(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1)
  (setq prescient-filter-method '(literal fuzzy regexp initialism)))

(use-package ivy-prescient
  :ensure t
  :after (ivy prescient counsel)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)
  :custom
  (ivy-prescient-enable-filtering t)
  (ivy-prescient-enable-sorting   t))

(require 'ivy)
(ivy-mode 1)

(require 'counsel)

;; Don't make M-x match on beginning of string
(add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))

(use-package company
  :ensure t
  :config
  (global-set-key (kbd "M-/") 'company-manual-begin)
  (global-company-mode 1)
  (setq company-idle-delay nil)
  (company-tng-mode)
  (setq company-selection-wrap-around t)
  ;; Gives me full control of when company auto completes. Keybindings
  ;; below don't work right otherwise.
  (setq copmany-auto-complete-chars nil)
  (company-flx-mode +1)

  :hook
  (org-mode . (lambda ()
		(setq-local company-idle-delay nil)))
  
  :bind
  ;; Source: https://chat.openai.com/c/aaa51304-65a0-4c69-b581-479984af4d51
  ;; 
  ;; In Emacs' company-mode, company-search-map and company-active-map
  ;; are keymaps used for different purposes within the completion
  ;; process.

  ;; company-search-map: This keymap is active when you are searching
  ;; for completions. It is used in the search phase, where you can
  ;; navigate through the available completions and select one. By
  ;; default, it inherits from company-active-map, so it includes the
  ;; key bindings defined in company-active-map. However, you can
  ;; customize company-search-map separately if you want to have
  ;; different key bindings specifically for the search phase.

  ;; company-active-map: This keymap is active when the completions
  ;; menu is displayed and you are interacting with it. It contains
  ;; key bindings that allow you to select and manipulate the
  ;; available completions. For example, you can use arrow keys, tab,
  ;; or numbers to navigate through the completions, select one, or
  ;; trigger additional actions. By default, company-active-map is the
  ;; main keymap for interacting with the completions menu.

  ;; In summary, company-search-map is used during the search phase to
  ;; navigate and select completions, while company-active-map is used
  ;; when the completions menu is active and you are interacting with
  ;; it. The latter includes the former by default, but you can
  ;; customize each map separately if desired.
  (:map company-active-map
	("M-/" . company-complete-common)
	("<RET>" . company-complete)
	("<return>" . company-complete)
	("[?\\t]" . company-complete-common-or-cycle))

  (:map company-search-map
	("<RET>" . company-complete-common-or-cycle)
	("<return>" . company-complete-common-or-cycle))
  )

(use-package company-ctags
  :ensure t

  :config
  (setq company-ctags-support-etags t)
  (setq company-ctags-fuzzy-match-p t)
  (with-eval-after-load 'company
    (company-ctags-auto-setup)))

(use-package company-wordfreq
  :ensure t)

;; Better sorting for company
(use-package company-flx
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t

  :config
  (projectile-mode 1)
  (projectile-load-known-projects)
  (setq projectile-tags-command "/usr/local/bin/ctags -Re -f \"%s\" %s")
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-current-project-on-switch 'keep))

(require 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; ;; (global-set-key (kbd "C-x M-f") 'counsel-projectile-find-file)
;; ;;(global-set-key (kbd "C-x M-s") 'counsel-projectile-ag)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; expand-region
;; ;;   https://github.com/emacsmirror/expand-region.git
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package expand-region
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-=") 'er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-complete-mode
;;   https://github.com/auto-complete/auto-complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  ;; Turn off automatic start of auto-complete
  (setq ac-auto-start nil)

  (add-hook 'auto-complete-mode-hook
          (lambda ()
            (local-set-key (kbd "M-/") 'auto-complete)
            (define-key ac-completing-map (kbd "C-n") 'ac-next)
            (define-key ac-completing-map (kbd "C-p") 'ac-previous)
            (define-key ac-completing-map (kbd "C-g") 'ac-stop)
            (define-key ac-completing-map (kbd "ESC") 'ac-stop))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; command-log-mode
;; ;;  https://github.com/lewang/command-log-mode.git
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package command-log-mode
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; undo-tree-mode
;; http://www.dr-qubit.org/git/undo-tree.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; eshell customizations
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun eshell/clear ()
;;   "Clear the eshell buffer."
;;   (interactive)
;;   (let ((inhibit-read-only t))
;;     (erase-buffer)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; haml-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package haml-mode
;;   :ensure t
;;   :config
;;   (add-hook 'haml-mode-hook
;;             (lambda ()
;;               (setq indent-tabs-mode nil)
;;               (define-key haml-mode-map "\C-m" 'newline-and-indent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hs-minor-mode keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'hs-minor-mode-hook
          (lambda ()
            (define-key hs-minor-mode-map (kbd "C-c h TAB") 'hs-toggle-hiding)
            (define-key hs-minor-mode-map (kbd "C-c h a") 'hs-hide-all)
            (define-key hs-minor-mode-map (kbd "C-c h s") 'hs-show-all)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; gherkin-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package gherkin-mode
;;   :ensure t)

;; (require 'gherkin-mode)
;; (add-to-list 'auto-mode-alist '("\\.gk$" . gherkin-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; csv-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :hook (csv-mode . (lambda () (csv-align-mode 1))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; inf-clojure
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The mainline version has a bug (#152) where it collapses
;; consecutive spaces. Although the bug itself bonkers, the maintainer
;; is too busy to investigate fully to fix it. Easier just to use my
;; own fork.
(el-get-bundle candera/inf-clojure)

(use-package inf-clojure)

(add-hook 'inf-clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (eldoc-mode -1)
	    ))

(require 'inf-clojure)

(defvar use-inf-clojure nil
  "If true, indicate that clojure-mode should also set up to use inf-clojure-minor-mode")

(defun inf-clojure-jack-in (dir cmd name)
  "Starts a new inf-clojure process and renames the resulting
  buffer to whatever inf-clojure-buffer is set to. To get the
  defaults to fill in correctly, use a .dir-locals.el that looks
  like this:

  ((clojure-mode
    (inf-clojure-buffer . \"requestinator-repl\")
    (use-inf-clojure-program . \"lein repl\")
    (use-inf-clojure . t)))
"
  (interactive (lexical-let* ((program (or use-inf-clojure-program "clojure"))
                              (buffer (or inf-clojure-buffer "*inf-clojure*"))
                              (dir (read-directory-name "Project directory: "
                                                        (locate-dominating-file
                                                          default-directory
                                                          ".git")
                                                        nil
                                                        t))
                              (_   (switch-to-buffer-other-window "*inf-clojure*"))
                              (_   (cd dir))
                              (cmd (read-string "Run Clojure: " program))
                              (name (read-buffer "REPL buffer name: " buffer)))
                 (when (get-buffer name)
                   (when (string= "y" (read-string "REPL buffer exists. Delete existing? (Y/n): " "y"))
                       (kill-buffer name)))
                 (list dir cmd name)))
  (inf-clojure cmd)
  (setq inf-clojure-buffer name)
  (rename-buffer name)
  ;; Re-enable inf-clojure-minor-mode in all open clojure-mode buffers
  ;; for this project, since eldoc and completion don't work if you
  ;; start the REPL later.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'clojure-mode)
                 (file-equal-p dir (inf-clojure-project-root)))
        (inf-clojure-minor-mode 1)))))

;; Make it so that I can set inf-clojure-buffer in a .dir-locals.el file
(put 'inf-clojure-buffer 'safe-local-variable #'stringp)
(put 'use-inf-clojure-program 'safe-local-variable #'stringp)
(put 'use-inf-clojure 'safe-local-variable #'booleanp)

;; Redefine C-c C-c since I always wind up killing the process
(defun comint-prevent-idiocy ()
  (interactive)
  (ding)
  (message "Command disabled because Craig is stupid. Use M-x <whatever> if you really meant it."))
(define-key inf-clojure-mode-map (kbd "C-c C-c") 'comint-prevent-idiocy)
(define-key inf-clojure-mode-map (kbd "C-c C-d") 'comint-prevent-idiocy)

(defun inf-clojure-refresh (start?)
  "Runs `(refresh)` in the attached REPL."
  (interactive "P")
  (let ((cur (current-buffer)))
    (pop-to-buffer inf-clojure-buffer)
    (goto-char (point-max))
    (insert (concat "(require 'clojure.tools.namespace.repl) (if-let [r (resolve 'user/reset)] (r) (clojure.tools.namespace.repl/refresh))"
                    (when start?
                      "(start)")))
    (comint-send-input)
    (pop-to-buffer cur)))

(define-key inf-clojure-minor-mode-map (kbd "M-R") 'inf-clojure-refresh)

;; I wonder if this is the source of the delays I would see in magit, ag, etc?
;; (add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; scad-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scad-mode
  :ensure t)

(require 'scad-mode)

(add-hook 'scad-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))

(defun scad-make-stl ()
  "Compiles the current file into an STL."
  (interactive)
  (async-shell-command (concat "makestl " (buffer-file-name))))

(define-key scad-mode-map (kbd "M-q")
  (lambda ()
    (interactive)
    (save-excursion
      (if (inside-comment-q)
          (fill-paragraph)
        (progn
          (mark-defun)
          (indent-for-tab-command))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Un-pork scrolling
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time
;; (setq compilation-scroll-output 'first-error) ;; Can also be `t` for
;;                                               ;; scrolling to the end

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Hoplon support
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))

;; (add-hook 'clojure-mode-hook
;;           '(lambda ()
;;              ;; Hoplon functions and macros
;;              (dolist (pair '((page . 'defun)
;;                              (loop-tpl . 'defun)
;;                              (cell-let . 'defun)
;;                              (if-tpl . '1)
;;                              (for-tpl . '1)
;;                              (for-append . '1)
;;                              (keyed-for-tpl . '2)
;;                              (map-lens-tpl . '2)
;;                              (do-watch . '1)
;;                              (case-tpl . '1)
;;                              (cond-tpl . 'defun)
;;                              (formula-of . 'defun)
;;                              (GET . 'defun)
;;                              (PUT . 'defun)
;;                              (POST . 'defun)))
;;                (put-clojure-indent (car pair)
;;                                    (car (last pair))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; iedit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package iedit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :ensure t)

(defun flyspell-popup-correct-previous-word ()
  (interactive)
  ;; This hack brought to you courtesy of
  ;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'flyspell-auto-correct-word) #'flyspell-popup-correct))
    (flyspell-auto-correct-previous-word (point))))

(use-package flyspell-popup
  :ensure t

  :bind
  (:map flyspell-mode-map
	("C-;" . 'flyspell-popup-correct-previous-word))

  :config
  (global-set-key (kbd "C-;") 'flyspell-popup-correct-previous-word))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; restclient
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; Not working yet - the execution is asynchronous
;; ;; (defun org-babel-execute:restclient (body params)
;; ;;   (save-mark-and-excursion
;; ;;    (with-temp-buffer
;; ;;      (insert body)
;; ;;      (restclient-http-send-current nil t)
;; ;;      (switch-to-buffer "*HTTP Response*")
;; ;;      (buffer-substring (point-min) (point-max)))))

;; (use-package restclient
;;   :ensure t)

;; (use-package ob-restclient
;;   :ensure t
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((restclient . t))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; csharp-mode
;; ;;
;; ;; Part of emacs as of v29
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Unfortunately I can't get these to work. They rely on map-put!
;; ;; which I think might only be available in Emacs 27.
;; (use-package tree-sitter :ensure t)
;; (use-package tree-sitter-langs :ensure t)

;; (use-package csharp-mode
;;   :ensure t
;;   :hook (csharp-mode-hook . (lambda ()
;;                               ;; (display-line-numbers-mode 1)
;;                               (setq c-basic-offset 2)))
;;   :config
;;   ;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
;;   (unless (assoc 'csharp-mode hs-special-modes-alist)
;;           (push '(csharp-mode
;;                   "{" "}"
;;                   "/[*/]" nil hs-c-like-adjust-block-beginning)
;;                   hs-special-modes-alist)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; java-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'java-mode-hook (lambda ()
;;                             (setq c-basic-offset 2
;;                                   tab-width 2
;;                                   indent-tabs-mode nil
;;                                   c-default-style '((java-mode . "linux")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sql-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sql-mode
  :mode "\\.p?sql$"
  :config
  ;; Not sure why this is not working
  (setq sql-use-indent-support nil)
  (sqlind-minor-mode -1))

;; (use-package sqlup-mode
;;   :ensure t)

(require 'sql)

;; (define-key sql-mode-map (kbd "C-M") 'newline)

;; (require 'sqlup-mode)

;; (add-hook 'sql-mode-hook
;;           (lambda ()
;; 	    (unless (string= "postgres" sql-product)
;; 	      (sqlup-mode 1))))

;; sql-eval-mode

(defvar sql-eval-mode-shell-buffer "")
(make-variable-buffer-local 'sql-eval-mode-shell-buffer)

(defvar sql-eval-mode-style :sqlcmd)
(make-variable-buffer-local 'sql-eval-mode-style)

(defun sql-to-single-line (sql)
  "Given a SQL string, returns a one-line version of that string."
  (replace-regexp-in-string "\n" " "
                            (replace-regexp-in-string "--.*" "" sql)))

(defun sql-eval-prep-input (sql)
  "Modifies the given SQL as appropriate for the current eval style."
  (concat "/* "
          (current-time-string)
          " */ "
          (replace-regexp-in-string "\t"
				    "  "
				    (if (eq sql-eval-mode-style :dosql)
					(sql-to-single-line sql)
				      sql))))

(defun sql-eval-get-separator (product)
  "Given a product, return a separator we can use between statements."
  (cond
   ((or (string= product "ansi")
	(string= product "ms")) "\nGO\n")
   ((string= product "postgres") ";\n")
   (t ";\n")))

(defvar sql-eval-term-processor :term)

(defun sql-eval-send-string (string)
  (case sql-eval-term-processor
    ((:term)
     (term-send-raw-string string)
     (term-send-raw-string "\n"))

    ((:vterm)
     (vterm-send-string string))

    ((:mistty)
     (mistty-send-string string))))

(defun sql-eval-buffer-subset (buf beg end)
  "Send the text in the buffer from `beg` to `end` to SQL eval buffer `buf`"
  (display-buffer buf)
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let ((cur (point)))
        (while (< cur end)
          (goto-char cur)
          (lexical-let* ((next-go (re-search-forward "^GO$" nil t))
                         (block-end (min end (if next-go
                                                 (- next-go 2)
                                               end)))
                         (sql (buffer-substring-no-properties cur block-end))
                         (prepped-sql (sql-eval-prep-input sql)))
            (save-window-excursion
              (switch-to-buffer-other-window buf)
	      (sql-eval-send-string prepped-sql)
              (when (eq sql-eval-mode-style :sqlcmd)
              	(sql-eval-send-string (sql-eval-get-separator sql-product))
		))
            (setq cur (if next-go (1+ next-go) block-end))))))))

(defun sql-eval-region (buffer)
  "Send the contents of the region to the *shell* buffer. Strips newlines from the string first.

With a prefix arg, prompts for the buffer to send to."
  (interactive "P")
  (let ((buf (if (and (null buffer) (not (string= "" sql-eval-mode-shell-buffer)))
                sql-eval-mode-shell-buffer
              (read-buffer "Buffer: " "sql-mdev-default" t))))
    (if (use-region-p)
        (sql-eval-buffer-subset buf
                                (region-beginning)
                                (region-end))
      (error "The region is not active - nothing to evaluate"))))

(defun sql-eval-buffer (buffer)
  "Send the contents of the buffer to the *shell* buffer. Strips newlines from the string first.

With a prefix arg, prompts for the buffer to send to."
  (interactive "P")
  (let ((buf (if (and (null buffer) (not (string= "" sql-eval-mode-shell-buffer)))
                sql-eval-mode-shell-buffer
              (read-buffer "Buffer: " "sql-mdev-default" t))))
    (sql-eval-buffer-subset buf
                            (point-min)
                            (point-max))))

(defun sql-eval-defun (buffer)
  "Send the text surrounding point (to the nearest blank line) to the *shell* buffer.

With a prefix arg, prompts for the buffer to send to."
  (interactive "P")
  (let ((buf (if (and (null buffer) (not (string= "" sql-eval-mode-shell-buffer)))
                 sql-eval-mode-shell-buffer
               (read-buffer "Buffer: " "sql-mdev-default" t))))
    (save-excursion
      (save-match-data
        (lexical-let* ((p (point))
                       (empty (search-backward-regexp "^\\s-*$" nil t))
                       (beg (if empty (1+ empty) (buffer-end -1)))
                       (_   (goto-char p))
                       (end (or (re-search-forward "^\\s-*$" nil t) (buffer-end 1))))
          (sql-eval-buffer-subset buf beg end))))))

(defun sql-eval-mode-lighter ()
  "Returns the value for the ligther to use when sql-eval-mode is enabled."
  (propertize
   (format " sql-eval[%s]" sql-eval-mode-shell-buffer)
   ;; TODO: make this customizable
   'face '(:foreground "lightblue")))

(make-variable-buffer-local 'sql-eval-mode-shell-buffer)

(defun sql-eval-set-buffer (buffer)
  "Sets the shell buffer that will be used to receive for commands sent from this buffer."
  (interactive "bBuffer:")
  (setq sql-eval-mode-shell-buffer buffer))

(defvar sql-eval-interpreter "zclsql")

(defvar sql-eval-mode-map (make-keymap))
(define-key sql-eval-mode-map (kbd "C-c e") 'sql-eval-region)
(define-key sql-eval-mode-map (kbd "C-c C-r") 'sql-eval-region)
(define-key sql-eval-mode-map (kbd "C-M-x") 'sql-eval-defun)
(define-key sql-eval-mode-map (kbd "C-c C-b") 'sql-eval-set-buffer)

(defvar sql-keywords
  '("A" "ABORT" "ABS" "ABSOLUTE" "ACCESS" "ACTION" "ADA" "ADD" "ADMIN" "AFTER" "AGGREGATE" "ALIAS" "ALL" "ALLOCATE" "ALSO" "ALTER"
    "ALWAYS" "ANALYSE" "ANALYZE" "AND" "ANY" "ARE" "ARRAY" "AS" "ASC" "ASENSITIVE" "ASSERTION" "ASSIGNMENT" "ASYMMETRIC" "AT" "ATOMIC" "ATTRIBUTE"
    "ATTRIBUTES" "AUDIT" "AUTHORIZATION" "AUTO_INCREMENT" "AVG" "AVG_ROW_LENGTH" "BACKUP" "BACKWARD" "BEFORE" "BEGIN" "BERNOULLI" "BETWEEN" "BIGINT" "BINARY" "BIT" "BIT_LENGTH"
    "BITVAR" "BLOB" "BOOL" "BOOLEAN" "BOTH" "BREADTH" "BREAK" "BROWSE" "BULK" "BY" "C" "CACHE" "CALL" "CALLED" "CARDINALITY" "CASCADE"
    "CASCADED" "CASE" "CAST" "CATALOG" "CATALOG_NAME" "CEIL" "CEILING" "CHAIN" "CHANGE" "CHAR" "CHAR_LENGTH" "CHARACTER" "CHARACTER_LENGTH" "CHARACTER_SET_CATALOG" "CHARACTER_SET_NAME" "CHARACTER_SET_SCHEMA"
    "CHARACTERISTICS" "CHARACTERS" "CHECK" "CHECKED" "CHECKPOINT" "CHECKSUM" "CLASS" "CLASS_ORIGIN" "CLOB" "CLOSE" "CLUSTER" "CLUSTERED" "COALESCE" "COBOL" "COLLATE" "COLLATION"
    "COLLATION_CATALOG" "COLLATION_NAME" "COLLATION_SCHEMA" "COLLECT" "COLUMN" "COLUMN_NAME" "COLUMNS" "COMMAND_FUNCTION" "COMMAND_FUNCTION_CODE" "COMMENT" "COMMIT" "COMMITTED" "COMPLETION" "COMPRESS" "COMPUTE" "CONDITION"
    "CONDITION_NUMBER" "CONNECT" "CONNECTION" "CONNECTION_NAME" "CONSTRAINT" "CONSTRAINT_CATALOG" "CONSTRAINT_NAME" "CONSTRAINT_SCHEMA" "CONSTRAINTS" "CONSTRUCTOR" "CONTAINS" "CONTAINSTABLE" "CONTINUE" "CONVERSION" "CONVERT" "COPY"
    "CORR" "CORRESPONDING" "COUNT" "COVAR_POP" "COVAR_SAMP" "CREATE" "CREATEDB" "CREATEROLE" "CREATEUSER" "CROSS" "CSV" "CUBE" "CUME_DIST" "CURRENT" "CURRENT_DATE" "CURRENT_DEFAULT_TRANSFORM_GROUP"
    "CURRENT_PATH" "CURRENT_ROLE" "CURRENT_TIME" "CURRENT_TIMESTAMP" "CURRENT_TRANSFORM_GROUP_FOR_TYPE" "CURRENT_USER" "CURSOR" "CURSOR_NAME" "CYCLE" "DATA" "DATABASE" "DATABASES" "DATE" "DATETIME" "DATETIME_INTERVAL_CODE" "DATETIME_INTERVAL_PRECISION"
    "DAY" "DAY_HOUR" "DAY_MICROSECOND" "DAY_MINUTE" "DAY_SECOND" "DAYOFMONTH" "DAYOFWEEK" "DAYOFYEAR" "DBCC" "DEALLOCATE" "DEC" "DECIMAL" "DECLARE" "DEFAULT" "DEFAULTS" "DEFERRABLE"
    "DEFERRED" "DEFINED" "DEFINER" "DEGREE" "DELAY_KEY_WRITE" "DELAYED" "DELETE" "DELIMITER" "DELIMITERS" "DENSE_RANK" "DENY" "DEPTH" "DEREF" "DERIVED" "DESC" "DESCRIBE"
    "DESCRIPTOR" "DESTROY" "DESTRUCTOR" "DETERMINISTIC" "DIAGNOSTICS" "DICTIONARY" "DISABLE" "DISCONNECT" "DISK" "DISPATCH" "DISTINCT" "DISTINCTROW" "DISTRIBUTED" "DIV" "DO" "DOMAIN"
    "DOUBLE" "DROP" "DUAL" "DUMMY" "DUMP" "DYNAMIC" "DYNAMIC_FUNCTION" "DYNAMIC_FUNCTION_CODE" "EACH" "ELEMENT" "ELSE" "ELSEIF" "ENABLE" "ENCLOSED" "ENCODING" "ENCRYPTED"
    "END" "END-EXEC" "ENUM" "EQUALS" "ERRLVL" "ESCAPE" "ESCAPED" "EVERY" "EXCEPT" "EXCEPTION" "EXCLUDE" "EXCLUDING" "EXCLUSIVE" "EXEC" "EXECUTE" "EXISTING"
    "EXISTS" "EXIT" "EXP" "EXPLAIN" "EXTERNAL" "EXTRACT" "FALSE" "FETCH" "FIELDS" "FILE" "FILLFACTOR" "FILTER" "FINAL" "FIRST" "FLOAT" "FLOAT4"
    "FLOAT8" "FLOOR" "FLUSH" "FOLLOWING" "FOR" "FORCE" "FOREIGN" "FORTRAN" "FORWARD" "FOUND" "FREE" "FREETEXT" "FREETEXTTABLE" "FREEZE" "FROM" "FULL"
    "FULLTEXT" "FUNCTION" "FUSION" "G" "GENERAL" "GENERATED" "GET" "GLOBAL" "GO" "GOTO" "GRANT" "GRANTED" "GRANTS" "GREATEST" "GROUP" "GROUPING"
    "HANDLER" "HAVING" "HEADER" "HEAP" "HIERARCHY" "HIGH_PRIORITY" "HOLD" "HOLDLOCK" "HOST" "HOSTS" "HOUR" "HOUR_MICROSECOND" "HOUR_MINUTE" "HOUR_SECOND" "IDENTIFIED" "IDENTITY"
    "IDENTITY_INSERT" "IDENTITYCOL" "IF" "IGNORE" "ILIKE" "IMMEDIATE" "IMMUTABLE" "IMPLEMENTATION" "IMPLICIT" "IN" "INCLUDE" "INCLUDING" "INCREMENT" "INDEX" "INDICATOR" "INFILE"
    "INFIX" "INHERIT" "INHERITS" "INITIAL" "INITIALIZE" "INITIALLY" "INNER" "INOUT" "INPUT" "INSENSITIVE" "INSERT" "INSERT_ID" "INSTANCE" "INSTANTIABLE" "INSTEAD" "INT"
    "INT1" "INT2" "INT3" "INT4" "INT8" "INTEGER" "INTERSECT" "INTERSECTION" "INTERVAL" "INTO" "INVOKER" "IS" "ISAM" "ISNULL" "ISOLATION" "ITERATE"
    "JOIN" "K" "KEY" "KEY_MEMBER" "KEY_TYPE" "KEYS" "KILL" "LANCOMPILER" "LANGUAGE" "LARGE" "LAST" "LAST_INSERT_ID" "LATERAL" "LEADING" "LEAST" "LEAVE"
    "LEFT" "LENGTH" "LESS" "LEVEL" "LIKE" "LIMIT" "LINENO" "LINES" "LISTEN" "LN" "LOAD" "LOCAL" "LOCALTIME" "LOCALTIMESTAMP" "LOCATION" "LOCATOR"
    "LOCK" "LOGIN" "LOGS" "LONG" "LONGBLOB" "LONGTEXT" "LOOP" "LOW_PRIORITY" "LOWER" "M" "MAP" "MATCH" "MATCHED" "MAX" "MAX_ROWS" "MAXEXTENTS"
    "MAXVALUE" "MEDIUMBLOB" "MEDIUMINT" "MEDIUMTEXT" "MEMBER" "MERGE" "MESSAGE_LENGTH" "MESSAGE_OCTET_LENGTH" "MESSAGE_TEXT" "METHOD" "MIDDLEINT" "MIN" "MIN_ROWS" "MINUS" "MINUTE" "MINUTE_MICROSECOND"
    "MINUTE_SECOND" "MINVALUE" "MLSLABEL" "MOD" "MODE" "MODIFIES" "MODIFY" "MODULE" "MONTH" "MONTHNAME" "MORE" "MOVE" "MULTISET" "MUMPS" "MYISAM" "NAME"
    "NAMES" "NATIONAL" "NATURAL" "NCHAR" "NCLOB" "NESTING" "NEW" "NEXT" "NO" "NO_WRITE_TO_BINLOG" "NOAUDIT" "NOCHECK" "NOCOMPRESS" "NOCREATEDB" "NOCREATEROLE" "NOCREATEUSER"
    "NOINHERIT" "NOLOGIN" "NONCLUSTERED" "NONE" "NORMALIZE" "NORMALIZED" "NOSUPERUSER" "NOT" "NOTHING" "NOTIFY" "NOTNULL" "NOWAIT" "NULL" "NULLABLE" "NULLIF" "NULLS"
    "NUMBER" "NUMERIC" "OBJECT" "OCTET_LENGTH" "OCTETS" "OF" "OFF" "OFFLINE" "OFFSET" "OFFSETS" "OIDS" "OLD" "ON" "ONLINE" "ONLY" "OPEN"
    "OPENDATASOURCE" "OPENQUERY"
    "OPENROWSET" "OPENXML" "OPERATION" "OPERATOR" "OPTIMIZE" "OPTION" "OPTIONALLY" "OPTIONS" "OR" "ORDER" "ORDERING" "ORDINALITY" "OTHERS" "OUT"
    "OUTER" "OUTFILE" "OUTPUT" "OVER" "OVERLAPS" "OVERLAY" "OVERRIDING" "OWNER" "PACK_KEYS" "PAD" "PARAMETER" "PARAMETER_MODE" "PARAMETER_NAME" "PARAMETER_ORDINAL_POSITION" "PARAMETER_SPECIFIC_CATALOG" "PARAMETER_SPECIFIC_NAME"
    "PARAMETER_SPECIFIC_SCHEMA" "PARAMETERS" "PARTIAL" "PARTITION" "PASCAL" "PASSWORD" "PATH" "PCTFREE" "PERCENT" "PERCENT_RANK" "PERCENTILE_CONT" "PERCENTILE_DISC" "PLACING" "PLAN" "PLI" "POSITION"
    "POSTFIX" "POWER" "PRECEDING" "PRECISION" "PREFIX" "PREORDER" "PREPARE" "PREPARED" "PRESERVE" "PRIMARY" "PRINT" "PRIOR" "PRIVILEGES" "PROC" "PROCEDURAL" "PROCEDURE"
    "PROCESS" "PROCESSLIST" "PUBLIC" "PURGE" "QUOTE" "RAID0" "RAISERROR" "RANGE" "RANK" "RAW" "READ" "READS" "READTEXT" "REAL" "RECHECK" "RECONFIGURE"
    "RECURSIVE" "REF" "REFERENCES" "REFERENCING" "REGEXP" "REGR_AVGX" "REGR_AVGY" "REGR_COUNT" "REGR_INTERCEPT" "REGR_R2" "REGR_SLOPE" "REGR_SXX" "REGR_SXY" "REGR_SYY" "REINDEX" "RELATIVE"
    "RELEASE" "RELOAD" "RENAME" "REPEAT" "REPEATABLE" "REPLACE" "REPLICATION" "REQUIRE" "RESET" "RESIGNAL" "RESOURCE" "RESTART" "RESTORE" "RESTRICT" "RESULT" "RETURN"
    "RETURNED_CARDINALITY" "RETURNED_LENGTH" "RETURNED_OCTET_LENGTH" "RETURNED_SQLSTATE" "RETURNS" "REVOKE" "RIGHT" "RLIKE" "ROLE" "ROLLBACK" "ROLLUP" "ROUTINE" "ROUTINE_CATALOG" "ROUTINE_NAME" "ROUTINE_SCHEMA" "ROW"
    "ROW_COUNT" "ROW_NUMBER" "ROWCOUNT" "ROWGUIDCOL" "ROWID" "ROWNUM" "ROWS" "RULE" "SAVE" "SAVEPOINT" "SCALE" "SCHEMA" "SCHEMA_NAME" "SCHEMAS" "SCOPE" "SCOPE_CATALOG"
    "SCOPE_NAME" "SCOPE_SCHEMA" "SCROLL" "SEARCH" "SECOND" "SECOND_MICROSECOND" "SECTION" "SECURITY" "SELECT" "SELF" "SENSITIVE" "SEPARATOR" "SEQUENCE" "SERIALIZABLE" "SERVER_NAME" "SESSION"
    "SESSION_USER" "SET" "SETOF" "SETS" "SETUSER" "SHARE" "SHOW" "SHUTDOWN" "SIGNAL" "SIMILAR" "SIMPLE" "SIZE" "SMALLINT" "SOME" "SONAME" "SOURCE"
    "SPACE" "SPATIAL" "SPECIFIC" "SPECIFIC_NAME" "SPECIFICTYPE" "SQL" "SQL_BIG_RESULT" "SQL_BIG_SELECTS" "SQL_BIG_TABLES" "SQL_CALC_FOUND_ROWS" "SQL_LOG_OFF" "SQL_LOG_UPDATE" "SQL_LOW_PRIORITY_UPDATES" "SQL_SELECT_LIMIT" "SQL_SMALL_RESULT" "SQL_WARNINGS"
   "SQLCA" "SQLCODE" "SQLERROR" "SQLEXCEPTION" "SQLSTATE" "SQLWARNING" "SQRT" "SSL" "STABLE" "START" "STARTING" "STATE" "STATEMENT" "STATIC" "STATISTICS" "STATUS"
    "STDDEV_POP" "STDDEV_SAMP" "STDIN" "STDOUT" "STORAGE" "STRAIGHT_JOIN" "STRICT" "STRING" "STRUCTURE" "STYLE" "SUBCLASS_ORIGIN" "SUBLIST" "SUBMULTISET" "SUBSTRING" "SUCCESSFUL" "SUM"
    "SUPERUSER" "SYMMETRIC" "SYNONYM" "SYSDATE" "SYSID" "SYSTEM" "SYSTEM_USER" "TABLE" "TABLE_NAME" "TABLES" "TABLESAMPLE" "TABLESPACE" "TEMP" "TEMPLATE" "TEMPORARY" "TERMINATE"
    "TERMINATED" "TEXT" "TEXTSIZE" "THAN" "THEN" "TIES" "TIME" "TIMESTAMP" "TIMEZONE_HOUR" "TIMEZONE_MINUTE" "TINYBLOB" "TINYINT" "TINYTEXT" "TO" "TOAST" "TOP"
    "TOP_LEVEL_COUNT" "TRAILING" "TRAN" "TRANSACTION" "TRANSACTION_ACTIVE" "TRANSACTIONS_COMMITTED" "TRANSACTIONS_ROLLED_BACK" "TRANSFORM" "TRANSFORMS" "TRANSLATE" "TRANSLATION" "TREAT" "TRIGGER" "TRIGGER_CATALOG" "TRIGGER_NAME" "TRIGGER_SCHEMA"
    "TRIM" "TRUE" "TRUNCATE" "TRUSTED" "TSEQUAL" "TYPE" "UESCAPE" "UID" "UNBOUNDED" "UNCOMMITTED" "UNDER" "UNDO" "UNENCRYPTED" "UNION" "UNIQUE" "UNKNOWN"
    "UNLISTEN" "UNLOCK" "UNNAMED" "UNNEST" "UNSIGNED" "UNTIL" "UPDATE" "UPDATETEXT" "UPPER" "USAGE" "USE" "USER" "USER_DEFINED_TYPE_CATALOG" "USER_DEFINED_TYPE_CODE" "USER_DEFINED_TYPE_NAME" "USER_DEFINED_TYPE_SCHEMA"
    "USING" "UTC_DATE" "UTC_TIME" "UTC_TIMESTAMP" "VACUUM" "VALID" "VALIDATE" "VALIDATOR" "VALUE" "VALUES" "VAR_POP" "VAR_SAMP" "VARBINARY" "VARCHAR" "VARCHAR2" "VARCHARACTER"
    "VARIABLE" "VARIABLES" "VARYING" "VERBOSE" "VIEW" "VOLATILE" "WAITFOR" "WHEN" "WHENEVER" "WHERE" "WHILE" "WIDTH_BUCKET" "WINDOW" "WITH" "WITHIN" "WITHOUT"
    "WORK" "WRITE" "WRITETEXT" "X509" "XOR" "YEAR" "YEAR_MONTH" "ZEROFILL" "ZONE"))

(define-minor-mode sql-highlight-minor-mode
  "A minor mode for highlighting SQL keywords."
  :init-value nil
  :lighter "highlight"
  :keymap nil
  (when sql-highlight-minor-mode
    (setq font-lock-keywords-case-fold-search t)
    (font-lock-add-keywords nil `((,(regexp-opt sql-keywords 'symbols) . font-lock-keyword-face)))
    (font-lock-fontify-buffer)))

(defvar clsql-keywords
  '())

(defvar clsql-record-separator
  "^\\*+ .*$")

(defvar clsql-keyword-specs
  `((,clsql-record-separator . font-lock-comment-face)
    ("^\\([A-Za-z_]+\\) *:" 1 font-lock-variable-name-face)))

(defun clsql-next-result ()
  "Moves to the next result in clsql output."
  (interactive)
  (re-search-forward clsql-record-separator))

(defun clsql-prev-result ()
  "Moves to the prior result in clsql output."
  (interactive)
  (re-search-backward clsql-record-separator))

(defvar clsql-mode-map (make-keymap))
(define-key clsql-mode-map (kbd "M-n") 'clsql-next-result)
(define-key clsql-mode-map (kbd "M-p") 'clsql-prev-result)
(define-key clsql-mode-map (kbd "q") 'bury-buffer)


(define-minor-mode clsql-minor-mode
  "A minor mode for highlighting cl-sql results and buffers."
  :init-value nil
  :lighter " clsql"
  :keymap clsql-mode-map
  (when clsql-minor-mode
    (setq font-lock-keywords-case-fold-search t)
    (font-lock-add-keywords nil (append `((,(regexp-opt clsql-keywords 'symbols) . font-lock-keyword-face))
					clsql-keyword-specs))
    (font-lock-fontify-buffer)))

(defun clsql-examine-last-result  ()
  "Clones the last results in the current buffer and highlights it assuming it is clsql output."
  (interactive)
  (lexical-let* ((prompt (cond
			  ((string= sql-product "postgres")
			   "^[[:alpha:]-]+=>")
			  (t "^[[:alpha:]-]+@[[:alnum:].-]+>")))
		 (b (get-buffer-create (concat (buffer-name) "-" (format-time-string "%m%d-%H%M%S"))))
		 (contents (save-excursion
			     (end-of-buffer)
			     (lexical-let* ((end (re-search-backward prompt))
					    (search1 (re-search-backward prompt nil t))
					    (search2 (re-search-forward prompt nil t))
					    (start (if search1 (point) (point-min))))
			       (buffer-substring-no-properties start end)))))
    (pop-to-buffer b)
    (insert contents)
    (beginning-of-buffer)
    (text-mode)
    (read-only-mode t)
    (clsql-minor-mode)
    (sql-highlight-minor-mode)))


(define-minor-mode sql-eval-mode
  "A minor mode for evaluating SQL statements by sending them to a comint buffer."
  :lighter (:eval (sql-eval-mode-lighter)))

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-eval-mode 1)))

(defun sql-eval-create-term ()
  "Create a terminal buffer. Return its name"
  (case sql-eval-term-processor
    ((:term)
     (progn
       (term "/usr/local/bin/bash")
       "*terminal*"))
    ;; TODO: Mistty and vterm if I ever need them
    ))

;; Old. Can be removed once I'm happy with the new setup
(defun db-eval-start-process-vterm (interpreter sql-product system &optional name envs)
  "Starts a shell that actually works with comint mode."
  ;; We need process-connection-type to be nil, or it chokes whenever
  ;; the input exceeds a certain length.
  (let* ((zone (read-string "Zone (mdev): " nil nil "mdev"))
         (deployment (read-string "Deployment (default): " nil nil "default"))
         (user (read-string "User (prod-user): " nil nil "prod-user"))
         (default-name (format "%s-%s-%s%s"
			       system
			       zone
			       deployment
			       (cond ((string= user "sa")
                                      "-sa")
				     ((string= user "readonly")
				      "-readonly")
                                     (""))))
         (name (or name (read-buffer (format "Buffer (%s): " default-name) default-name)))
         (process-connection-type nil)
         (temp-name (symbol-name (gensym)))
         ;; (process (make-comint temp-name "bash" nil "-i"))
         (starred-name (concat "*" temp-name "*"))
         ;; (temp-file (make-temp-file "encrypt" nil nil "throwaway"))
         (continue     (if (get-buffer name)
			 (lexical-let ((answer (read-string (format "A buffer named %s already exists. (K)ill and recreate, (s)witch to, or (a)bort? " name))))
			   (cond
			    ((string= answer "k") (progn (kill-buffer name) t))
			    ((string= answer "s") (progn (switch-to-buffer name) nil))
			    (t nil)))
			 t)))
    (when continue 
      (save-window-excursion
	(vterm starred-name))
      (switch-to-buffer-other-window starred-name)
      ;; Override keys that vterm does weird stuff with
      (local-set-key (kbd "M-N") 'candera-next-window)
      (local-set-key (kbd "M-P") 'candera-previous-window)

      (rename-buffer name)
      (sql-set-product sql-product)
      (vterm-set-min-window-width 1000)
      ;; Somehow inf-clojure is setting this variable in my SQL Eval
      ;; buffers, which is screwing things up royally. Clobber it back.
      (make-variable-buffer-local 'comint-input-sender)
      (setq comint-input-sender 'comint-simple-send)
      ;; This is a hack to get gpg-agent to have the keys we need. I
      ;; haven't been able to figure out how to get zerkenv to do it
      ;; correctly on its own when run under emacs
      ;; (epa-decrypt-file "~/dummy.asc" "/dev/null")
      ;; The sleeps give the prompt a chance to fully print, since otherwise we get weird coloring.
      ;; (sleep-for 0.25)
;;       (process-send-string vterm--process "bash -i\n")
      (process-send-string vterm--process (format "bash -i -c 'zerk; emacs-%s-setup --zone %s --deployment-name %s --user %s'\n" system zone deployment user)))))

(defun db-eval-start-process (interpreter sql-product system &optional name envs)
  "Starts a shell that actually works with comint mode."
  ;; We need process-connection-type to be nil, or it chokes whenever
  ;; the input exceeds a certain length.
  (let* ((zone (read-string "Zone (mdev): " nil nil "mdev"))
         (deployment (read-string "Deployment (default): " nil nil "default"))
         (user (read-string "User (prod-user): " nil nil "prod-user"))
         (default-name (format "%s-%s-%s%s"
			       system
			       zone
			       deployment
			       (cond ((string= user "sa")
                                      "-sa")
				     ((string= user "readonly")
				      "-readonly")
                                     (""))))
         (name (or name (read-buffer (format "Buffer (%s): " default-name) default-name)))
         (process-connection-type nil)
         (temp-name (symbol-name (gensym)))
         ;; (process (make-comint temp-name "bash" nil "-i"))
         (starred-name (concat "*" temp-name "*"))
         ;; (temp-file (make-temp-file "encrypt" nil nil "throwaway"))
         (continue     (if (get-buffer name)
			 (lexical-let ((answer (read-string (format "A buffer named %s already exists. (K)ill and recreate, (s)witch to, or (a)bort? " name))))
			   (cond
			    ((string= answer "k") (progn (kill-buffer name) t))
			    ((string= answer "s") (progn (switch-to-buffer name) nil))
			    (t nil)))
			 t)))
    (when continue 
      (save-window-excursion
	(switch-to-buffer (sql-eval-create-term))
	(rename-buffer starred-name))
      (switch-to-buffer-other-window starred-name)
      ;; Override keys that vterm does weird stuff with
      (local-set-key (kbd "M-N") 'candera-next-window)
      (local-set-key (kbd "M-P") 'candera-previous-window)

      (rename-buffer name)
      (sql-set-product sql-product)
      ;; (vterm-set-min-window-width 1000)
      ;; Somehow inf-clojure is setting this variable in my SQL Eval
      ;; buffers, which is screwing things up royally. Clobber it back.
      (make-variable-buffer-local 'comint-input-sender)
      (setq comint-input-sender 'comint-simple-send)
      ;; This is a hack to get gpg-agent to have the keys we need. I
      ;; haven't been able to figure out how to get zerkenv to do it
      ;; correctly on its own when run under emacs
      ;; (epa-decrypt-file "~/dummy.asc" "/dev/null")
      ;; The sleeps give the prompt a chance to fully print, since otherwise we get weird coloring.
      ;; (sleep-for 0.25)
      ;;       (process-send-string vterm--process "bash -i\n")
      (save-window-excursion
	(switch-to-buffer name)
	(local-set-key (kbd "C-c C-e") 'clsql-examine-last-result)
	(local-set-key (kbd "C-c e") 'clsql-examine-last-result)
	(sql-eval-send-string (format "bash -i -c 'zerk; emacs-%s-setup --zone %s --deployment-name %s --user %s'\n" system zone deployment user))))))

(defun sql-eval-start-process (interpreter &optional name envs)
  "Starts a shell that actually works with comint mode. Defaults
to `sql-eval-interpreter` for interpreter."
  (interactive "P")
  (let* ((interpreter (if (null interpreter)
			  sql-eval-interpreter
			(read-string "Interpreter: "))))
    (db-eval-start-process interpreter "ms" "sql" name envs)))

(defun itemdb-eval-start-process ()
  "Starts a shell for use with ItemDB."
  (interactive)
  (db-eval-start-process "psql" "postgres" "itemdb"))

(defun duckdb-eval-start-process ()
  "Starts a shell for use with DuckDB."
  (interactive)
  (db-eval-start-process "duckdb" "postgres" "duckdb"))

(define-key sql-mode-map (kbd "C-c s") 'sql-eval-start-process)

;; SQL Org Babel support

(defun org-babel-edit-prep:sql (babel-info)
  "Get an org src buffer ready to edit SQL. Looks for header
variables `:buffer-name` and `:eval-buffer`, which set the name
of the buffer and `sql-eval-mode-shell-buffer` in the Org source
buffer, respectively."
  (let* ((arguments (nth 2 babel-info))
         (buffer-name (alist-get :buffer-name arguments))
         (eval-buffer (alist-get :eval-buffer arguments))
         (sql-product-val (alist-get :engine arguments))
         (style (alist-get :sql-eval-mode-style arguments)))
    (when buffer-name
      (rename-buffer (format "*Org src %s*" buffer-name)))
    (when eval-buffer
      (sql-eval-set-buffer eval-buffer))
    (when sql-product
      (sql-set-product sql-product-val))
    (when style
      (setq sql-eval-mode-style (intern-soft style)))))

(defun org-babel-execute:sql (body params)
  "Execute SQL. This function is called by `org-babel-execute-src-block`."
  (with-temp-buffer
    (insert body)
    (shell-execute )
    (sql-eval-buffer-subset (alist-get :eval-buffer params) (point-min) (point-max))))

(defun buffer-contains (search)
  "Returns true if the buffer contains the regexp `search`, nil otherwise."
  (goto-char (point-min))
  (re-search-forward search nil t))

(defun org-babel-execute:sql (body params)
  "Execute SQL. This function is called by `org-babel-execute-src-block`."
  (save-excursion
    (org-babel-mark-block)
    (lexical-let ((sql (buffer-substring (mark) (point))))
      (with-temp-buffer
        (lexical-let ((ns (open-network-stream "SQL" (current-buffer) "localhost" 7788)))
          (process-send-string ns sql)
          (process-send-string ns "\nGO\n")
          (while (not (buffer-contains "^:end$"))
            (accept-process-output ns 0.1))
          (delete-process ns)
          (read (buffer-substring-no-properties (point-min) (point-max)))
	  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adzerk stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pinentry
  :ensure t)

(require 'epa-file)

(setq epa-file-name-regexp "\\.\\(gpg\\)\\|\\(asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
(epa-file-enable)
(epa-file-name-regexp-update)

;; Start the pinentry server for use from shells
(pinentry-start)

;; WARNING: Nonsense required:
;; - Uninstall all gpg stuff
;; - brew install gnupg21
;; - brew link --force gnupg21
;; - ln -s /usr/local/bin/gpg2 /usr/local/bin/gpg

;; This is what lets emacs do the password prompt, instead of that
;; weird terminal popup thing. Also need allow-emacs-pinentry in
;; ~/.gnupg/gpg-agent.conf
(setq epa-pinentry-mode 'loopback)

(defun get-adzerk-var (name)
  (save-mark-and-excursion
   (lexical-let ((temp-file (make-temp-file "adzerk")))
     (message temp-file)
     (unwind-protect
         (progn
           (epa-decrypt-file "~/.adzerk-defaults.asc" temp-file)
           (with-temp-buffer
             (insert-file-contents temp-file)
             (goto-char 0)
             (re-search-forward (format "^export %s=" (regexp-quote name)))
             (lexical-let ((start (point)))
               (end-of-line)
               (buffer-substring start (point)))))
       (delete-file temp-file)))))

(defvar adzerk-api-key-cache nil)

(defun adzerk-api-key ()
  (interactive)
  (or adzerk-api-key-cache
      (setq adzerk-api-key-cache (get-adzerk-var "ADZERK_API_KEY"))))

(defun read-gpg-file-to-string (filename)
  "Decrypt the GPG-encrypted FILENAME and return its contents as a string.
Uses a temporary file and ensures it is deleted afterward."
  (let ((tempfile (make-temp-file "emacs-gpg-decrypt-")))
    (unwind-protect
        (progn
          ;; Decrypt into a temporary buffer
          (with-temp-buffer
            (let ((coding-system-for-read 'binary)) ; ensure no decoding issues
              (epa-decrypt-file filename tempfile))
	    (insert-file-contents tempfile)
            (buffer-string)))
      ;; Always clean up
      (delete-file tempfile))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; mirror-image support
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Lets you edit images in text in one buffer and see the changes live
;; ;; in another. Source:
;; ;; https://emacs.stackexchange.com/questions/7198/indirect-buffer-in-image-mode-main-buffer-in-text

;; (defun gpc/mirror-buffer (buffer-name &optional more-after-change)
;;   "Create a buffer whose contents will follow the current one's
;; and returns the new buffer.  Runs `more-after-change' after each
;; change if provided.

;; This differs from `clone-indirect-buffer' in that the new buffer
;; is not visiting a file.  It's really just a kludge to support
;; `gpc/mirror-image', which see."
;;   (interactive (list
;;                 (let ((default (concat (buffer-name) "<mirror>")))
;;                   (read-string "Buffer name: " default
;;                                nil nil default))))
;;   (make-local-variable 'after-change-functions)
;;   (make-local-variable 'kill-buffer-hook)
;;   (lexical-let*
;;       ((target-buffer (generate-new-buffer buffer-name))
;;        ;; Give lexical scope to arg
;;        (after-change more-after-change)
;;        (copy-change
;;         #'(lambda(start end old-len)
;;             (let ((inhibit-read-only t))
;;               ;; Quick and dirty: may not be suitable for large buffers.
;;               (copy-to-buffer target-buffer (point-min) (point-max))
;;               (when (functionp after-change)
;;                 (funcall after-change target-buffer))))))

;;     ;; Initialize the target buffer with the source text.
;;     (copy-to-buffer target-buffer (point-min) (point-max))

;;     (add-hook 'after-change-functions copy-change t t)

;;     ;; Cleanup hooks.

;;     ;; Kill the other buffer if the source buffer is closed.
;;     (add-hook 'kill-buffer-hook
;;               #'(lambda () (kill-buffer target-buffer)) t t)

;;     ;; Destroy the change hook if the other buffer is killed.
;;     (with-current-buffer target-buffer
;;       (make-local-variable 'kill-buffer-hook)
;;       (add-hook 'kill-buffer-hook
;;                 #'(lambda ()
;;                     (remove-hook 'after-change-functions copy-change t))
;;                 t t))))

;; (defun gpc/mirror-image ()
;;   "Open an `image-mode' buffer that tracks the content of the
;; current buffer.  Intended for use with svg files."
;;   (interactive)
;;   (image-mode-as-text)
;;   (let* ((buffer-name (concat (buffer-name) "<image>"))
;;          ;; An `image-mode' buffer will switch back to text when its contents
;;          ;; are replaced.  Besides, the image is not updated in-place when the
;;          ;; content changes, so you'd have to toggle back to image-mode anyway.
;;          (after-change '(lambda (buffer)
;;                           (with-current-buffer buffer (image-mode))))
;;          (mirror (gpc/mirror-buffer buffer-name after-change)))
;;     (split-window)
;;     (other-window 1)
;;     (switch-to-buffer buffer-name)
;;     (image-mode)
;;     (other-window 1)))

;; (dir-locals-set-class-variables
;;  'pubconsumer
;;  '((clojure-mode .
;;                  ((use-inf-clojure-program . "nc localhost 51336")
;;                   (inf-clojure-buffer . "pubconsumer-repl")
;;                   (use-inf-clojure . t)))))

;; (dir-locals-set-directory-class
;;  "~/adzerk/pubconsumer/" 'pubconsumer)

(dir-locals-set-class-variables
 'vmt
 '((clojure-mode
    (use-inf-clojure-program . "nc localhost 8728")
    (inf-clojure-buffer . "vmt-repl")
    (use-inf-clojure . t))))

(dir-locals-set-directory-class
 "~/projects/vmt/" 'vmt)

(dir-locals-set-class-variables
 'vmtfx
 '((clojure-mode
    (use-inf-clojure-program . "nc localhost 51363")
    (inf-clojure-buffer . "vmtfx-repl")
    (use-inf-clojure . t))))

(dir-locals-set-directory-class
 "~/projects/vmtfx/" 'vmtfx)

(dir-locals-set-class-variables
 'publisher
 '((clojure-mode .
                 ((use-inf-clojure-program . "nc localhost 5555")
                  (inf-clojure-buffer . "publisher-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/publisher/" 'publisher)

(dir-locals-set-class-variables
 'api-proxy
 '((clojure-mode .
                 ((use-inf-clojure-program . "nc localhost 5555")
                  (inf-clojure-buffer . "api-proxy-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/api-proxy/" 'api-proxy)

(dir-locals-set-class-variables
 'audit-log-writer
 '((clojure-mode .
                 ((use-inf-clojure-program . "nc localhost 9584")
                  (inf-clojure-buffer . "audit-log-writer-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/audit-log-writer/" 'audit-log-writer)

(dir-locals-set-class-variables
 'integration-tests
 '((clojure-mode .
                 ((use-inf-clojure-program . "nc localhost 5272")
                  (inf-clojure-buffer . "integration-tests-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/integration-tests/" 'integration-tests)

(dir-locals-set-class-variables
 'pipeline-sandbox
 '((clojure-mode .
                 ((use-inf-clojure-program . "clojure")
                  (inf-clojure-buffer . "pipeline-sandbox-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/pipeline-sandbox/" 'pipeline-sandbox)

(dir-locals-set-class-variables
 'adset-manager
 '((clojure-mode .
                 ((use-inf-clojure-program . "nc localhost 5555")
                  (inf-clojure-buffer . "adset-manager-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/adset-manager/" 'adset-manager)

(dir-locals-set-class-variables
 'custom-relevancy
 '((clojure-mode .
                 ((use-inf-clojure-program . "nc localhost 5555")
                  (inf-clojure-buffer . "custom-relevancy-repl")
                  (use-inf-clojure . t)))))

(dir-locals-set-directory-class
 "~/adzerk/custom-relevancy/" 'custom-relevancy)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; image+
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package image+
;;   :ensure t
;;   :bind (:map imagex-sticky-mode-map
;;               ("+" . imagex-sticky-zoom-in)
;;               ("-" . imagex-sticky-zoom-out)
;;               ("g" . imagex-sticky-restore-original)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Edit server - edit in emacs from Chrome
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'edit-server)
;; ;; (edit-server-start)


;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;
;; ;; ;; Atomic Chrome - edit with Emacs from Chrome
;; ;; ;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (use-package atomic-chrome
;; ;;   :ensure t
;; ;;   :config
;; ;;   (atomic-chrome-start-server))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Miscellaneous customizations
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq confirm-nonexistent-file-or-buffer nil)

;;(load "~/.emacs.d/custom/colors.el")

(add-to-list 'auto-mode-alist '("\\.az$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(setq save-interprogram-paste-before-kill t)

;; First indent, then complete
(setq tab-always-indent 'complete)

(defun clear-tags-tables ()
  "Removes all tags from the active tags lists."
  (interactive)
  (setq tags-table-list '()))

;; This prevents Emacs from creating lockfiles, which prevent two
;; people from editing the same file at the same time. Since my setup
;; is single-user, all this does is occasionally confuse me by telling
;; me a file is locked, and would I like to steal it?
(setq create-lockfiles nil)

(defun align-to-commas ()
  "Aligns regions as if they were a CSV."
  (interactive)
  (let ((buffer-invisibility-spec '()))
    (csv-align-fields nil (region-beginning) (region-end))))

(defun edit-json-string-in-other-buffer ()
  "Opens a temporary buffer and populates it with the contents of
the string at point. Hitting C-c C-c in that buffer will save it
back to the original string."
  (interactive)
  (save-mark-and-excursion
    (lexical-let* ((orig (current-buffer))
                   (start (region-beginning))
                   (end (region-end))
                   (contents (buffer-substring-no-properties start end))
		   (finish (lambda ()
                             (interactive)
                             (save-mark-and-excursion
                               (lexical-let* ((contents (buffer-substring-no-properties
                                                         (point-min)
                                                         (point-max))))
                                 (switch-to-buffer orig)
                                 (kill-region start end)
                                 (goto-char start)
                                 (insert contents))))))
      (lexical-let* ((new-buffer (switch-to-buffer (make-temp-name "json-string")))))
      (insert contents)
      (replace-string "\\n" "")
      (javascript-mode)
      (local-set-key (kbd "C-c C-c") finish)
      (local-set-key (kbd "C-c C-f") finish))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mac customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dock-mode nil)

(defun docked-laptop-mode ()
  "When the laptop is connected to my DasKeyboard, different
  bindings work better."
  (interactive)
  ;; (setq ns-command-modifier 'meta)      ; Command key is meta
  (setq ns-command-modifier 'super)     ; Command key is super
  ;; (setq mac-right-option-modifier 'meta)
  (setq mac-right-option-modifier 'hyper)
  (setq mac-right-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  ;; (setq mac-option-modifier 'super)
  ;; Sadly, doesn't seem to work
  ;; (setq ns-function-modifier 'hyper)
  (setq dock-mode 'docked))

(defun undocked-laptop-mode ()
  "When the laptop isn't connected to my DasKeyboard, different
  bindings work better."
  (interactive)
  ;; (setq ns-command-modifier 'meta)      ; Command key is meta
  (setq ns-command-modifier 'super)     ; Command key is super
  (setq mac-right-option-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq dock-mode 'undocked))

;; Never really got this to work right
;; (defvar dock-mode-timer
;;   (when (or (string= system-name "valinor")
;;             (string= system-name "valinor.local")
;;             (string= system-name "brightmoon")
;;             (string= system-name "brightmoon.local"))
;;     (run-with-timer
;;      0 
;;      5
;;      (lambda ()
;;        (lexical-let ((currently-undocked (> 2000 (display-pixel-width))))
;;          (when (and (not (eq dock-mode 'undocked))
;;                     currently-undocked)
;;            (undocked-laptop-mode))
;;          (when (and (not (eq dock-mode 'docked))
;;                     (not currently-undocked))
;;            (docked-laptop-mode)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Compensate for the fact that I frequently fat-finger C-m instead of
;; ;; C-n by first mapping C-m away from return (which will only work
;; ;; when in a GUI session), and then binding C-m
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Caused more problems than it solved.
;; ;; (define-key input-decode-map [?\C-m] [C-m])
;; ;; (global-set-key (kbd "<C-m>") 'next-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prog-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package prog-mode
  :hook
  ;; Show glyphless chars
  (prog-mode . glyphless-display-mode)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; highlight-focus
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (el-get-bundle kriyative/highlight-focus)

;; (use-package highlight-focus
;;   :config
;;   ;; set the background of the mode-line
;;   (setq highlight-focus:face 'default
;;         highlight-focus:face-property :background
;;         highlight-focus:face-property-value "black"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; floobits
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package floobits
;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; blockdiag-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package blockdiag-mode
;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; graphviz-dot-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package graphviz-dot-mode
;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; org-mind-map
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-mind-map
;;   :ensure t
;;   :init (require 'ox-org))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; plantuml-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package plantuml-mode
;;   :ensure t
;;   :config
;;   (progn
;;     (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.0/libexec/plantuml.jar")
;;     (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.0/libexec/plantuml.jar")
;;     (add-to-list
;;      'org-src-lang-modes '("plantuml" . plantuml))
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      '((plantuml . t)))))

;; ;; (use-package flycheck-plantuml
;; ;;   :ensure t
;; ;;   :config
;; ;;   (flycheck-plantuml-setup))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; neotree
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun neotree-resize-window (&rest _args)
;;     "Resize neotree window.
;; https://github.com/jaypei/emacs-neotree/pull/110"
;;     (interactive)
;;     (neo-buffer--with-resizable-window
;;      (let ((fit-window-to-buffer-horizontally t))
;;        (fit-window-to-buffer))))

;; (use-package neotree
;;   :ensure t
;;   :config
;;   (add-hook 'neo-change-root-hook #'neotree-resize-window)
;;   (add-hook 'neo-enter-hook #'neotree-resize-window))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; php-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package php-mode
;;   :ensure t)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; web-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package web-mode
;;   :ensure t
;;   :mode "\\.spark\\'")

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;
;; ;; ;; interleave-mode
;; ;; ;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; Allows you to take notes on a PDF while reading it in Emacs.

;; ;; (use-package interleave
;; ;;   :ensure t)

;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;;
;; ;; ;; Capture snippet
;; ;; ;;
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; ;; Credit http://ul.io/nb/2018/04/30/better-code-snippets-with-org-capture/

;; ;; Unfortunately which-function-mode has a bug wherein
;; ;; `which-func-update` will totally bork exiting an ediff mode,
;; ;; forever displaying "selecting deleted buffer" and making Emacs
;; ;; impossible to use.

;; ;; (which-function-mode 1)

;; ;; (defun nb/org-capture-get-src-block-string (major-mode)
;; ;;   "Given a major mode symbol, return the associated org-src block
;; ;; string that will enable syntax highlighting for that language

;; ;; E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

;; ;;   (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
;; ;;     (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

;; ;; (defun nb/org-capture-region ()
;; ;;   (interactive)
;; ;;   (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
;; ;;         (func-name (which-function))
;; ;;         (file-name (buffer-file-name))
;; ;;         (line-number (line-number-at-pos (region-beginning)))
;; ;;         (org-src-mode (nb/org-capture-get-src-block-string major-mode)))
;; ;;     (kill-new (format
;; ;;                "file:%s::%s
;; ;; In ~%s~:
;; ;; #+BEGIN_SRC %s
;; ;; %s
;; ;; #+END_SRC"
;; ;;                file-name
;; ;;                line-number
;; ;;                func-name
;; ;;                org-src-mode
;; ;;                code-snippet))))

;; ;; ;; ;; Capture Template
;; ;; ;; ("s" "code snippet" entry (file ,(my/org-dir-file "snippets.org"))
;; ;; ;;  "* %?\n%(my/org-capture-code-snippet \"%F\")")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Load a theme
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customize it with (customize-create-theme 'candera nil)

(load-theme 'candera t)

;; (use-package haki-theme
;;   :ensure t
;;   :config
;;   (setq haki-region "#2e8b6d"
;; 	;; If you skip setting this, it will use 'default' font.
;; 	haki-heading-font "Maple Mono"
;; 	haki-sans-font "Iosevka Comfy Motion"
;; 	haki-title-font "Impress BT"
;; 	haki-link-font "VictorMono Nerd Font" ;; or Maple Mono looks good
;; 	haki-code-font "Maple Mono") ;; inline code/verbatim (org,markdown..)
;;   (load-theme 'haki t)
;;   (custom-theme-set-faces 'haki
;; 			  '(link ((t (:slant normal))))
;; 			  '(org-priority ((t (:foreground "grey")))))
;;   (set-default-font-size 200))

;; (use-package paganini-theme
;;   :ensure t
;;   :config
;;   (load-theme 'paganini t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; clubhouse integration
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package dash
;;   :ensure t)

;; (use-package dash-functional
;;   :ensure t)

;; (use-package clubhouse-api
;;   :config
;;   (setq clubhouse-api-team-name "adzerk")
;;   (setq clubhouse-api-default-project "Management")
;;   (setq clubhouse-api-auth-token-path "~/.adzerk/secrets/candera/CLUBHOUSE_API_TOKEN.asc")
;;   (add-hook 'clubhouse-api-story-edit-minor-mode-hook
;;             (lambda ()
;;               (visual-line-mode 1))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Simple line references
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun save-line-reference ()
;;   "Saves a reference to the current line (like `foo.clj(23)` to the kill ring.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; git-link: link to a line on GitHub
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package git-link
  :ensure t
  :init
  (setq git-link-use-commit t))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; visible-mark: make mark visible in buffers
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (comment
;;  (use-package visible-mark
;;    :ensure t
;;    :init
;;    (defface visible-mark-active ;; put this before (require 'visible-mark)
;;      '((((type tty) (class mono)))
;;        (t (:box "magenta"))) "")
;;    (defface custom-visible-mark-face1
;;      '((((type tty) (class mono)))
;;        (t (:box "light red")))
;;      "Example face which can be customized and added to subsequent face lists.")
;;    (defface custom-visible-mark-face2
;;      '((((type tty) (class mono)))
;;        (t (:box "light green")))
;;      "Example face which can be customized and added to subsequent face lists.")

;;    :config
;;    (global-visible-mark-mode 1)
;;    (setq visible-mark-max 2)
;;    (setq visible-mark-faces `(custom-visible-mark-face1 custom-visible-mark-face2))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Make Tramp faster
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq remote-file-name-inhibit-cache nil)
;; (setq vc-ignore-dir-regexp
;;       (format "%s\\|%s"
;;                     vc-ignore-dir-regexp
;;                     tramp-file-name-regexp))
;; (setq tramp-verbose 3)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; ediff
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package ediff
;;   :custom
;;   (ediff-window-setup-function 'ediff-setup-windows-plain) ; No separate frame
;;   (ediff-diff-options "-w")
;;   (ediff-split-window-function 'split-window-horizontally) ; Default to horizontal split
;;   )

;; ;; TODO: disable projectile in tramp buffers if things are still slow. Somehow.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Paradox: (hopefully) better package UI
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package paradox
;;   :ensure t

;;   :config
;;   (paradox-enable))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; copy-as-format: copy regions of emacs buffers to the kill ring in
;; ;; various formats.
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package copy-as-format
;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; github-review: support for reviewing pull requests from within
;; ;; emacs.
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package github-review
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; forge: pull request support for magit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sqlite3
  :ensure t)

(use-package forge
  :ensure t

  :bind
  (:map forge-topic-mode-map
	("r" . forge-topic-set-review-requests)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; multiple-cursors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; ggtags
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (use-package ggtags
;; ;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; ejc-sql
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Complexity alert! Requires nREPL, Cider, Lein....
;; ;; (use-package ejc-sql
;; ;;   :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; camp-letterize
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://concordia.campintouch.com/v2/login/login.aspx?ReturnUrl=%2f

(defun camp-letterize ()
  "Formats a letter to the kids at camp in a way that's
compatible with the Concordia web sysstem."
  (interactive)
  (unless (region-active-p)
      (lexical-let ((prior-point (point)))
	(search-backward "-----------")
	(next-line)
	(beginning-of-line)
	(set-mark (point))
	(goto-char prior-point)
	(search-forward "----------")
	(beginning-of-line)))
  (lexical-let ((end-marker (region-end)))
    (kill-ring-save (mark) (point))
    (lexical-let ((buffer-name (format "camp-letter-%s.txt" (format-time-string "%F")))
                  (line-count (count-lines (mark) (point)))
                  (chunk-size 45))
      (switch-to-buffer buffer-name)
      (yank)
      (beginning-of-buffer)
      (lexical-let ((page-number 1))
        (while (< (point) (point-max))
          (ignore-errors
            (next-line chunk-size)
            (re-search-backward "^$")
            (setq page-number (1+ page-number))
            (insert (format "\n[Continued in part %d]\n\n[Part %d]\n"
                            page-number
                            page-number)))))
      (set-fill-column 10000)
      (beginning-of-buffer)
      (set-mark (point))
      (end-of-buffer)
      (fill-paragraph nil t)
      (beginning-of-buffer)
      (set-mark (point))
      (end-of-buffer)
      (kill-ring-save (mark) (point))
      (message "Buffer saved to kill ring")
      (browse-url "https://concordia.campintouch.com/Email07/"))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Telega - Emacs Telegram client
;; ;;
;; ;; https://github.com/zevlg/telega.el
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (require 'cursor-sensor)
;; ;; (setq load-path (append '("~/projects/telega.el" load-path)))

;; ;; (require 'telega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; vterm
;; Terminal emulator for emacs via libvterm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun toggle-vterm-copy-mode ()
  (interactive)
  (if vterm-copy-mode
      (vterm-copy-mode-done nil)
    (vterm-copy-mode 1)))

(defun vterm-clear-all ()
  "Clears both the buffer and the scrollback"
  (interactive)
  (vterm-clear)
  (vterm-clear-scrollback))

(defun vterm-set-min-window-width (min-width)
  "Sets the minimum window width in vterm buffers."
  (interactive "nBuffer size: ")
  (setq-local vterm-min-window-width min-width))

(use-package vterm
  :ensure t
  ;; Maybe I should just set this when I need it.
  ;; :custom
  ;; (vterm-min-window-width 1000 "I can scroll with emacs if I want to.")
  :config
  (make-variable-buffer-local 'global-hl-line-mode)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq global-hl-line-mode nil)
	      (global-set-key (kbd "C-c C-t") 'toggle-vterm-copy-mode)
	      (local-set-key (kbd "C-c C-t") 'toggle-vterm-copy-mode)
	      (local-set-key (kbd "M-N") 'candera-next-window)
	      (local-set-key (kbd "M-P") 'candera-previous-window)))

  :bind
  (:map vterm-mode-map
	("C-c C-t" . toggle-vterm-copy-mode)
	("C-c C-l" . vterm-clear-all)
	("C-c C-c" . vterm-send-C-c)
	("M-N" . nil)
	("M-P" . nil)
	("C-c e" . clsql-examine-last-result))
  (:map vterm-copy-mode-map
	("C-c C-t" . toggle-vterm-copy-mode)
	("C-c C-l" . vterm-clear-all)
	("C-c C-c" . vterm-send-C-c)
	("M-N" . nil)
	("M-P" . nil)
	("C-c e" . clsql-examine-last-result)))

(defadvice vterm-copy-mode (after my-vterm-copy-mode-advice (arg) activate)
  "Set the cursor type according to whether we're in copy mode or not."
  (if vterm-copy-mode
      (setq cursor-type 'box)
    (setq cursor-type 'bar)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; gcode-mode
;; ;;
;; ;; http://pixpopuli.blogspot.com/2011/01/syntax-highlighting-for-cnc-g-code.html
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'generic-x)

;; (define-generic-mode gcode-generic-mode
;;   '(";" ("(" . ")"))
;;   (apply 'append
;;          (mapcar #'(lambda (s) (list (upcase s) (downcase s) (capitalize s)))
;;                  '("sub" "endsub" "if" "do" "while" "endwhile" "call" "endif"
;;                    "sqrt" "return" "mod" "eq" "ne" "gt" "ge" "lt" "le" "and"
;;                    "or" "xor" "atan" "abs" "acos" "asin" "cos" "exp"
;;                    "fix" "fup" "round" "ln" "sin" "tan" "repeat" "endrepeat")))
;;   '(; ("\\(;.*\\)" (1 font-lock-comment-face))
;;     ("\\(#<_?[A-Za-z0-9_]+>\\)" (1 font-lock-type-face))
;;     ("\\([NnGgMmFfSsTtOo]\\)" (1 font-lock-function-name-face))
;;     ("\\([XxYyZzAaBbCcUuVvWwIiJjKkPpQqRr]\\)" (1 font-lock-string-face))
;;     ("\\([\-+]?[0-9]*\\.[0-9]+\\)" (1 font-lock-constant-face))
;;     ("\\(#[0-9]+\\)" (1 font-lock-type-face))
;;     ("\\([0-9]+\\)" (1 font-lock-constant-face)))
;;   '("\\.ngc\\'" "\\.gcode\\'" )
;;   nil
;;   "Generic mode for g-code files.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; A better json-pretty-print
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun candera-json-pretty-print-buffer ()
;;   "Pretty prints the entire buffer"
;;   (interactive)
;;   (shell-command-on-region (point-min) (point-max) "jq ." (current-buffer)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; edit-region-in-other-buffer
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun edit-region-in-other-buffer (edit-mode)
;;   "Opens a temporary buffer and populates it with the contents of
;; the region. Hitting C-c C-c in that buffer will save it
;; back to the original string."
;;   (interactive "amode: ")
;;   (save-mark-and-excursion
;;     (lexical-let* ((orig (current-buffer))
;;                    (start (region-beginning))
;;                    (end (region-end))
;;                    (contents (buffer-substring-no-properties start end)))
;;       (switch-to-buffer (make-temp-name "edit-string"))
;;       (eval (list edit-mode))
;;       (use-local-map (if (current-local-map)
;;                          (copy-keymap (current-local-map))
;;                        (make-sparse-keymap)))
;;       (local-set-key (kbd "C-c C-c") (lambda ()
;;                                        (interactive)
;;                                        (save-mark-and-excursion
;;                                          (lexical-let* ((contents (buffer-substring-no-properties
;;                                                                    (point-min)
;;                                                                    (point-max))))
;;                                            (switch-to-buffer orig)
;;                                            (kill-region start end)
;;                                            (goto-char start)
;;                                            (insert contents)))))
;;       (insert contents))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; lilypond-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq load-path (append '("/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/")
;;                         load-path))
;; ;; Copied from /Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/lilypond-init.el
;; (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
;; (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
;; (add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))
;; (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; hyperbole
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package hyperbole
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; which-key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun candera-which-key-show-keymap (map)
  "Show the which-key interface for the current mode.

With a prefix arg, prompts for the mode to show."
  (interactive "P")
  (lexical-let* ((mode-name (if (null map)
				(symbol-name major-mode)
			      (read-string "Mode: " (symbol-name major-mode))))
		 (mode-map-name  (concat mode-name "-map"))
		 (mode-map-symbol (intern mode-map-name))
		 (mode-map (symbol-value mode-map-symbol)))
    (if (keymapp mode-map) 
	(which-key-show-keymap mode-map-symbol)
      (message "Could not find map %s" mode-map-name))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (global-set-key (kbd "C-c K") 'candera-which-key-show-keymap)
  (global-set-key (kbd "C-c k") 'which-key-show-top-level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; exec-path-from-shell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ag
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; coffee-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package coffee-mode
  :ensure t

  ;; TODO: Not sure this works
  :after eglot

  :config
  (add-to-list 'eglot-server-programs '(coffee-mode . ("coffeesense-language-server" "--stdio"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; bitly-shorten
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bitly-shorten ()
  "Shortens link at point via bitly."
  (interactive)
  (let ((bounds (thing-at-point-bounds-of-url-at-point)))
    (save-mark-and-excursion
      (shell-command-on-region (car bounds) (cdr bounds) "bitly-shorten"nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; define-word
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package define-word
  :ensure t)

(defun flyspell-goto-previous-error ()
  "Go to the previous previously detected error.
In general FLYSPELL-GOTO-NEXT-ERROR must be used after
FLYSPELL-BUFFER."
  (interactive)
  (let ((pos (point))
	(min (point-min)))
    (if (and (eq (current-buffer) flyspell-old-buffer-error)
	     (eq pos flyspell-old-pos-error))
	(progn
	  (if (= flyspell-old-pos-error min)
	      ;; goto end of buffer
	      (progn
		(message "Restarting from end of buffer")
		(goto-char (point-max)))
	    (backward-word 1))
	  (setq pos (point))))
    ;; seek the next error
    (while (and (> pos min)
		(let ((ovs (overlays-at pos))
		      (r '()))
		  (while (and (not r) (consp ovs))
		    (if (flyspell-overlay-p (car ovs))
			(setq r t)
		      (setq ovs (cdr ovs))))
		  (not r)))
      (setq pos (1- pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (if (= pos min)
	(message "No more miss-spelled word!"))))

(defun define-previous-erroneous-word ()
  "Print a definition of the most recent previously flagged word,
so we can check to see if flyspell is just lacking a definition."
  (interactive)
  (save-mark-and-excursion
    (flyspell-goto-previous-error)
    (define-word-at-point nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; slack mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encrypted-file-contents (path)
  (save-mark-and-excursion
   (lexical-let ((temp-file (make-temp-file "epa-temp")))
     (message temp-file)
     (unwind-protect
         (progn
           (epa-decrypt-file path temp-file)
           (lexical-let ((s (with-temp-buffer
			      (insert-file-contents temp-file)
			      (buffer-string))))
	     (delete-file temp-file)
	     s))))))


(el-get-bundle yuya373/emacs-slack)
;; (el-get-bundle yuya373/helm-slack) ;; optional
;; (use-package helm-slack :after (slack)) ;; optional

(use-package alert
  :ensure t)
(use-package circe
  :ensure t)
(use-package oauth
  :ensure t)
(use-package request
  :ensure t)
(use-package websocket
  :ensure t)

;; Get token following instructions at https://github.com/yuya373/emacs-slack/blob/9fbad25af792d1dee88aae0fc9550bd29dabe269/README.md

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "Kevel"
   :default t
   ;; :token (encrypted-file-contents "~/.config/slack-token.asc")
   ;; :cookie (encrypted-file-contents "~/.config/slack-cookie.asc")
   :subscribed-channels '(teammgmt)
   :full-and-display-names t)

  ;; (slack-register-team
  ;;  :name "test"
  ;;  :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
  ;;  :subscribed-channels '(hoge fuga))

  ;; (evil-define-key 'normal slack-info-mode-map
  ;;   ",u" 'slack-room-update-messages)
  ;; (evil-define-key 'normal slack-mode-map
  ;;   ",c" 'slack-buffer-kill
  ;;   ",ra" 'slack-message-add-reaction
  ;;   ",rr" 'slack-message-remove-reaction
  ;;   ",rs" 'slack-message-show-reaction-users
  ;;   ",pl" 'slack-room-pins-list
  ;;   ",pa" 'slack-message-pins-add
  ;;   ",pr" 'slack-message-pins-remove
  ;;   ",mm" 'slack-message-write-another-buffer
  ;;   ",me" 'slack-message-edit
  ;;   ",md" 'slack-message-delete
  ;;   ",u" 'slack-room-update-messages
  ;;   ",2" 'slack-message-embed-mention
  ;;   ",3" 'slack-message-embed-channel
  ;;   "\C-n" 'slack-buffer-goto-next-message
  ;;   "\C-p" 'slack-buffer-goto-prev-message)
  ;;  (evil-define-key 'normal slack-edit-message-mode-map
  ;;   ",k" 'slack-message-cancel-edit
  ;;   ",s" 'slack-message-send-from-buffer
  ;;   ",2" 'slack-message-embed-mention
  ;;   ",3" 'slack-message-embed-channel

) 

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; jet - pretty print EDN
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jet ()
  "Pretty print EDN in region"
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code-review - magit-like code reviews
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package code-review
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sdcv-mode - StarDict dictionary support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle gucong/emacs-sdcv)

(use-package sdcv-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; edit-with-emacs hammerspoon support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/dmgerman/editWithEmacs.spoon

(load-file "~/.hammerspoon/Spoons/editWithEmacs.spoon/hammerspoon.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eglot - LSP server integration. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disabled 2023-10-09 because it was hanging emacs when opening files via TRAMP 

;; ;; ;; Unfortunately the bundled version of project is old, overrides the
;; ;; ;; one eglot depends on, and does not contain the project-root var.
;; ;; ;; This is a backport workaround from
;; ;; ;; [[here][https://github.com/hlissner/doom-emacs/issues/3269#issuecomment-637945554]]
;; ;; (defun project-root (project)
;; ;;   (car (project-roots project)))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook
;; 	    (lambda ()
;; 	      (eglot-ensure))))

;; As of [2023-11-02] Breaks paredit

;; Tried again [2025-02-11]. Hopefully better than lsp-bridge
;; 
;; It's built-in now: no need to ensure
(use-package eglot
  :hook
  ((python-mode . eglot-ensure)       ; Python
   (js-mode . eglot-ensure)           ; JavaScript
   (typescript-mode . eglot-ensure)   ; TypeScript
   (c-mode . eglot-ensure)            ; C
   (rust-mode . eglot-ensure)         ; Rust
   (clojure-mode . eglot-ensure)	; Clojure
   )
  :config
  (setq eglot-autoshutdown t) ;; Shutdown unused servers automatically
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright")))
  )

;; I don't like that docstrings show up in the echo area
(use-package eldoc-box
  :ensure t
  :bind
  (:map eglot-mode-map
	("C-h C-b" . eldoc-box-help-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; corfu - lightweight (supposedly) popup completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; lsp-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;; (clojure-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)
;; ;; if you are helm user
;; ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy
;;   :ensure t
;;   :commands lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :ensure t)

;; ;; Add other languages with dap-LANGUAGE
;; ;; Instructions for Java here: https://emacs-lsp.github.io/dap-mode/page/clojure/
;; (use-package dap-java
;;   :ensure t)

;; ;; optional if you want which-key integration
;; (use-package which-key
;;   :config
;;   (which-key-mode))


;; Alternative formulation from https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; lsp-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package lsp-mode
;;   :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lsp-bridge
;;
;; Supposedly faster LSP client
;;
;; https://github.com/manateelazycat/lsp-bridge
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package yasnippet
;;   :ensure t)

;; (use-package markdown-mode
;;   :ensure t)

;; (defun lsp-bridge-popup-diagnostic ()
;;   "Pops up a diagnostic for an error at point, if any."
;;   (interactive)
;;   (lsp-bridge-diagnostic-maybe-display-error-at-point))

;; (use-package lsp-bridge
;;   :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;;             :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;             :build (:not compile))
;;   :init
;;   (global-lsp-bridge-mode)

;;   :config
;;   (setq lsp-bridge-enable-with-tramp nil)
;;   (setq lsp-bridge-complete-manually t)
;;   (company-mode -1)

;;   :bind
;;   (:map lsp-bridge-mode-map
;; 	("M-." . lsp-bridge-find-def)
;; 	("M-," . lsp-bridge-find-def-return)
;; 	("M-/" . lsp-bridge-popup-complete-menu)
;; 	("s-e" . lsp-bridge-popup-diagnostic)
;; 	("s-n" . lsp-bridge-diagnostic-jump-next)
;; 	("s-p" . lsp-bridge-diagnostic-jump-prev)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; shell-script-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sh-mode-hook (lambda () (auto-complete-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; straight
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elfeed
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-elfeed-sort-newest-first t
  "Whether to sort elfeed entries newest first.")

(defun my-elfeed-toggle-sort-order ()
  "Toggle between newest-first and oldest-first sorting."
  (interactive)
  (setq my-elfeed-sort-newest-first (not my-elfeed-sort-newest-first))
  (if my-elfeed-sort-newest-first
      (setq elfeed-search-sort-function 
            (lambda (a b) (> (elfeed-entry-date a) (elfeed-entry-date b))))
    (setq elfeed-search-sort-function 
          (lambda (a b) (< (elfeed-entry-date a) (elfeed-entry-date b)))))
  (elfeed-search-update--force)
  (message "Elfeed sort order: %s first" 
           (if my-elfeed-sort-newest-first "newest" "oldest")))

(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
	      ("/" . elfeed-search-set-filter)
	      ("s" . my-elfeed-toggle-sort-order)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; elfeed-tube
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package elfeed-tube
;;   :ensure t
;;   ;; :straight (:host github :repo "karthink/elfeed-tube")
;;   :after elfeed
;;   :demand t
;;   :config
;;   ;; (setq elfeed-tube-auto-save-p nil) ;; t is auto-save (not default)
;;   ;; (setq elfeed-tube-auto-fetch-p t) ;;  t is auto-fetch (default)
;;   (elfeed-tube-setup)

;;   :bind (:map elfeed-show-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)
;;          :map elfeed-search-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gcode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ===== Mach3 G-code help in Emacs (gcode-mode) ===============================

;; Core dictionary (Mach3 mill/router focus; many lathe codes included but marked)
(defvar gcode-mach3-docs
  '(;; --- Motion / Interp / Planes / Units ---
    ("G0"    . "Rapid positioning")
    ("G00"   . "Rapid positioning (same as G0)")
    ("G1"    . "Linear interpolation (feed move)")
    ("G01"   . "Linear interpolation (feed move)")
    ("G2"    . "CW arc move (I/J/K for center; R for radius)")
    ("G02"   . "CW arc move (I/J/K for center; R for radius)")
    ("G3"    . "CCW arc move (I/J/K for center; R for radius)")
    ("G03"   . "CCW arc move (I/J/K for center; R for radius)")
    ("G4"    . "Dwell (P seconds or X seconds in Mach3)")
    ("G04"   . "Dwell (P seconds or X seconds in Mach3)")
    ("G5"    . "Cubic spline (Mach3 optional/plug-in; often unsupported)")
    ("G17"   . "Select XY plane")
    ("G18"   . "Select ZX plane")
    ("G19"   . "Select YZ plane")
    ("G20"   . "Units: inches")
    ("G21"   . "Units: millimeters")
    ("G28"   . "Return to machine home (via optional intermediate in G91)")
    ("G28.1" . "Home axis/axes (Mach3: execute homing for specified axes)")
    ("G30"   . "Return to secondary reference position")
    ("G40"   . "Cancel cutter radius compensation (CRC)")
    ("G41"   . "CRC left of path (requires D/tool dia table)")
    ("G41.1" . "Dynamic CRC left using specified D value")
    ("G42"   . "CRC right of path (requires D/tool dia table)")
    ("G42.1" . "Dynamic CRC right using specified D value")
    ("G43"   . "Apply tool length offset (H#). Z moves are to tool tip.")
    ("G43.1" . "Set tool length offset to given value directly (Z comp)")
    ("G49"   . "Cancel tool length compensation")
    ("G52"   . "Local coordinate system shift (temporary work offset)")
    ("G53"   . "Move in machine coordinates (non-modal)")
    ("G54"   . "Work offset 1")
    ("G55"   . "Work offset 2")
    ("G56"   . "Work offset 3")
    ("G57"   . "Work offset 4")
    ("G58"   . "Work offset 5")
    ("G59"   . "Work offset 6")
    ("G59.1" . "Extended work offset 7 (Mach3)")
    ("G59.2" . "Extended work offset 8 (Mach3)")
    ("G59.3" . "Extended work offset 9 (Mach3)")
    ("G61"   . "Exact stop mode (no blending at corners)")
    ("G61.1" . "Exact path mode (Mach3)")
    ("G64"   . "Constant velocity mode (path blending)")
    ("G68"   . "Coordinate rotation (Mach3; A = angle, about current origin)")
    ("G69"   . "Cancel coordinate rotation")
    ("G70"   . "[Lathe] Finish cycle or inch units (Fanuc). Mach3 mill: N/A.")
    ("G71"   . "[Lathe] Roughing cycle or mm units (Fanuc). Mach3 mill: N/A.")
    ("G73"   . "High-speed peck drilling cycle")
    ("G80"   . "Cancel canned cycle")
    ("G81"   . "Drill (simple) canned cycle")
    ("G82"   . "Counterbore (drill with dwell) canned cycle")
    ("G83"   . "Peck drilling canned cycle")
    ("G84"   . "Right-hand tapping cycle (requires sync / rigid tapping config)")
    ("G85"   . "Bore, feed in / feed out")
    ("G86"   . "Bore, feed in / spindle stop / rapid out")
    ("G88"   . "Bore, manual dwell / manual retract (rarely used)")
    ("G89"   . "Bore with dwell, feed in / feed out")
    ("G90"   . "Absolute distance mode (programmed coords are absolute)")
    ("G91"   . "Incremental distance mode")
    ("G91.1" . "Arc center mode: I/J/K incremental (Mach3 default)")
    ("G92"   . "Set current position (coord system shift)")
    ("G92.1" . "Cancel G92 offsets")
    ("G92.2" . "Suspend G92 (store), set shift to zero")
    ("G92.3" . "Restore previously suspended G92")
    ("G94"   . "Feed rate: units per minute")
    ("G95"   . "Feed rate: units per revolution (Mach3 supports with spindle)")
    ("G96"   . "[Lathe] Constant surface speed (CSS)")
    ("G97"   . "[Lathe] Cancel CSS; use RPM")
    ("G98"   . "Canned cycle return to initial level")
    ("G99"   . "Canned cycle return to R level")
    ;; --- Probing (Mach3) ---
    ("G31"   . "Probe move (stop on trip). Axes with distances move at F.")
    ;; --- Misc / Overrides / Subroutines ---
    ("M0"    . "Program stop")
    ("M00"   . "Program stop")
    ("M1"    . "Optional stop (executes if optional stop enabled)")
    ("M01"   . "Optional stop (executes if optional stop enabled)")
    ("M2"    . "Program end")
    ("M02"   . "Program end")
    ("M3"    . "Spindle on clockwise (S = RPM)")
    ("M03"   . "Spindle on clockwise (S = RPM)")
    ("M4"    . "Spindle on counter-clockwise")
    ("M04"   . "Spindle on counter-clockwise")
    ("M5"    . "Spindle stop")
    ("M05"   . "Spindle stop")
    ("M6"    . "Tool change (T# must be set)")
    ("M06"   . "Tool change (T# must be set)")
    ("M7"    . "Mist coolant on (if configured)")
    ("M07"   . "Mist coolant on (if configured)")
    ("M8"    . "Flood coolant on")
    ("M08"   . "Flood coolant on")
    ("M9"    . "Coolant off")
    ("M09"   . "Coolant off")
    ("M30"   . "Program end and rewind")
    ("M48"   . "Enable feed/speed overrides")
    ("M49"   . "Disable feed/speed overrides")
    ("M98"   . "Call subprogram (P=sub id, L=repeat)")
    ("M99"   . "Return from subprogram / loop to start in subs")
    ;; --- Parameters / Notes (not codes but helpful) ---
    ("I"     . "Arc center X or incremental X distance (with G2/G3)")
    ("J"     . "Arc center Y or incremental Y distance (with G2/G3)")
    ("K"     . "Arc center Z or incremental Z distance (with G2/G3)")
    ("R"     . "Arc radius (alternative to IJK); canned-cycle R-plane")
    ("P"     . "Dwell seconds (G4); subprogram ID (M98); canned-cycle params")
    ("Q"     . "Peck depth (e.g., G83)")
    ("F"     . "Feed rate (G94 units/min, G95 units/rev)")
    ("S"     . "Spindle speed (RPM)")
    ("T"     . "Tool number")
    ("H"     . "Tool length offset register (for G43/G43.1)")
    ("D"     . "Diameter offset register (for G41/G42)")
    )
  "Alist mapping Mach3 G/M codes (strings) to short descriptions.")

(defun gcode-mach3--normalize-token (tok)
  "Normalize TOK like \"G01\" -> \"G1\"; keep decimals, upcase."
  (when tok
    (let* ((u (upcase tok))
           (m (string-match "\\`\\([GMT]\\)0*\\([0-9]+\\)\\(\\.[0-9]+\\)?\\'" u)))
      (if m
          (concat (match-string 1 u)
                  (number-to-string (string-to-number (match-string 2 u)))
                  (or (match-string 3 u) ""))
        u))))

(defun gcode-mach3--code-at-point ()
  "Return G/M/T code token at point like G1, G91.1, M3, T6, or parameter I/J/K/R/P."
  (save-excursion
    (let ((sym (thing-at-point 'symbol t)))
      ;; If point isn't on the token, try scanning around
      (or (and sym
               (when (string-match-p "\\`[GMTgmt][0-9]+\\(\\.[0-9]+\\)?\\'\\|\\`[IJKRPFSDHTD]\\'" sym)
                 sym))
          (progn
            (skip-chars-backward "A-Za-z0-9._")
            (when (looking-at "[GMTgmt][0-9]+\\(\\.[0-9]+\\)?\\|[IJKRPFSDHTD]")
              (match-string 0)))))))

(defun gcode-mach3-lookup (token)
  "Lookup TOKEN in `gcode-mach3-docs' with normalization and fallbacks."
  (let* ((norm (gcode-mach3--normalize-token token)))
    (or (cdr (assoc norm gcode-mach3-docs))
        (cdr (assoc (upcase token) gcode-mach3-docs)))))

(defun gcode-mach3-describe-at-point ()
  "Echo a short Mach3 description for the code/param at point."
  (interactive)
  (let* ((tok (gcode-mach3--code-at-point))
         (desc (and tok (gcode-mach3-lookup tok))))
    (if desc
        (message "%s — %s" (gcode-mach3--normalize-token tok) desc)
      (message "No Mach3 doc for token at point."))))

(defun gcode-mach3-describe (code)
  "Prompt for a code (e.g., G43, M3, G83) and show its description."
  (interactive
   (list (completing-read "Mach3 code: "
                          (delete-dups (mapcar #'car gcode-mach3-docs))
                          nil t)))
  (let* ((norm (gcode-mach3--normalize-token code))
         (desc (gcode-mach3-lookup code)))
    (if desc
        (message "%s — %s" norm desc)
      (message "No Mach3 doc for %s" norm))))

;; Eldoc integration: show help automatically in minibuffer
(defun gcode-mach3-eldoc ()
  (let* ((tok (gcode-mach3--code-at-point))
         (desc (and tok (gcode-mach3-lookup tok))))
    (when desc
      (format "%s — %s" (gcode-mach3--normalize-token tok) desc))))

(defun gcode-mach3-eldoc-setup ()
  (setq-local eldoc-documentation-function #'gcode-mach3-eldoc)
  (eldoc-mode 1)
  ;; Handy key to describe token under cursor:
  (local-set-key (kbd "C-c h") #'gcode-mach3-describe-at-point)
  ;; And a prompt-based describe:
  (local-set-key (kbd "C-c H") #'gcode-mach3-describe))

(use-package gcode
  :straight (:host github :repo "jasapp/gcode-emacs")
  :config (eldoc-box-hover-at-point-mode 1)
  :mode (("\\.tap)?$" . gcode-mode)
	 ("\\.TAP)?$" . gcode-mode))
  :hook (gcode-mode . gcode-mach3-eldoc-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; multi-vterm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package multi-vterm
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; json-navigator
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Augments json tree navigation
(use-package tree-mode
  :ensure t)

(use-package json-navigator
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-remark
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-remark
  :ensure t

  :pin gnu

  :config
  (org-remark-global-tracking-mode +1)
  (define-key global-map (kbd "C-c n m") #'org-remark-mark)

  :bind
  (:map org-remark-mode-map
	("C-c n o" . #'org-remark-open)
	("C-c n ]" . #'org-remark-view-next)
	("C-c n [" . #'org-remark-view-prev)
	("C-c n r" . #'org-remark-remove)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dictionary-search (built in)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uses the 1913 Webster edition, advocated here:
;; https://irreal.org/blog/?p=10867
(setq dictionary-server "dict.org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; wc-mode
;;
;; Running count of words in buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wc-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; visual-fill-column
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Usually you use this with visual-line-mode
(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; asciidoc mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package adoc-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-roam
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-roam
;;   :ensure t
;;   :config
;;   (org-roam-db-autosync-enable)
;;   (require 'org-roam-protocol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; log4j-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package log4j-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; logview
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package logview
  :ensure t
  :config
  (make-variable-buffer-local 'datetime-timezone)
  (setq datetime-timezone  "Amerca/New_York")
  (setq logview-additional-submodes
	'(("format"
	   (format . "TIMESTAMP [THREAD] LEVEL NAME {} ")
	   (levels . "SLF4J")
	   (timestamp)
	   (aliases)))))

(defun logview-json-follow-mode-next ()
  "Move to next log message and disply it in the popup buffer."
  (interactive)
  (logview-next-entry)
  (lexical-let ((b (current-buffer))
		(exists (get-buffer "log-message")))
    (save-mark-and-excursion
      (set-mark (point))
      (end-of-line)
      (kill-ring-save (point) (mark))
      (pop-to-buffer "log-message")
      (unless exists
	(javascript-mode))
      (beginning-of-buffer)
      (yank)
      (set-mark (point))
      (end-of-buffer)
      (kill-region (point) (mark))
      (unwind-protect
	  (json-pretty-print-buffer))
      (beginning-of-buffer)
      (transient-mark-mode -1))
    (pop-to-buffer b)))

(defvar logview-json-follow-mode-map (make-keymap))

(define-key logview-json-follow-mode-map (kbd "n") 'logview-json-follow-mode-next)

(define-minor-mode logview-json-follow-mode
  "A minor mode for following the current log message when
navigating a logview buffer."
  :keymap logview-json-follow-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; grammarly
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Doesn't work with TRAMP because Ubuntu 18 is too old

;; (use-package lsp-grammarly
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-grammarly)
;;                        (lsp))))  ; or lsp-deferred

;; (use-package flycheck-grammarly
;;   :ensure t)

(use-package langtool
  :ensure t
  :config
  ;; (setq langtool-language-tool-jar "/Users/candera/bin/languagetool/languagetool-commandline.jar")
  ;;(setq langtool-language-tool-server-jar "  ~/bin/languagetool/languagetool-server.jar")
  (setq langtool-language-tool-server-jar "/opt/homebrew/opt/languagetool/libexec/languagetool-server.jar")
)

;; (use-package langtool-popup
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mistty
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2023-10-15 doesn't work. Filed https://github.com/szermatt/mistty/issues/9

(use-package mistty
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tengwar (Elvish) face
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x buffer-face-set tengwar-annatar
(make-face 'tengwar-annatar)
(set-face-font 'tengwar-annatar "-*-Tengwar Annatar-regular-normal-normal-*-*-*-*-*-p-0-iso10646-1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Orderless
;; 
;; Provides completion without regard to order. The trick here is to
;; use space instead of dash in e.g. M-x completion. So type `M-x org
;; table insert`, not `M-x org-table-insert` to match everything with
;; those words regardless of order.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I didn't like the way it messed up fuzzy matching

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(initials orderless basic))
;;   (completion-category-overrides '((file (styles fuzzy basic partial-completion))))
;;   :config
;;   (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
;;   (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yeetube
;;
;; YouTube integration in Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yeetube
  :ensure t
  :config
  (setf yeetube-display-thumbnails t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inf-clojure-eval-ns-defun ()
  "Evaluates the top-level at point in the namespace of its buffer."
  (interactive)
  (save-mark-and-excursion
    (mark-defun)
    (lexical-let ((expr (buffer-substring-no-properties (point) (mark)))
		  (ns (clojure-find-ns)))
      (inf-clojure--send-string inf-clojure-buffer (format "(in-ns '%s)" ns))
      (inf-clojure--send-string inf-clojure-buffer expr)
      (inf-clojure--send-string inf-clojure-buffer "(in-ns 'user)"))))

(defun clojure-pretty-print-region ()
  "Pretty-prints the region. Requires use of external tool `jet`."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "jet --pretty" :replace t))

(use-package clojure-mode
  :ensure t
  :bind
  :config
  (when (fboundp 'lsp-bridge-mode)
    (lsp-bridge-mode 1))

  :bind
  (:map clojure-mode-map
	("C-M-z" . inf-clojure-eval-ns-defun)
	("M-/" . complete-tag)
	("C-M-/" . hippie-expand)
	("s-n" . flymake-goto-next-error)
	("s-p" . flymake-goto-prev-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; typescript-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; casual-dired: Interactive menu for dired
;;
;; http://yummymelon.com/devnull/announcing-casual-dired---an-opinionated-porcelain-for-the-emacs-file-manager.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package casual-dired
  :ensure t
  :bind
  (:map dired-mode-map
	("C-o" . casual-dired-tmenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Eat: emulate a terminal
;;
;; https://codeberg.org/akib/emacs-eat
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eat
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-package-update: update packages on startup
;;
;; https://github.com/rranelli/auto-package-update.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Slows startup down way too much
;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pdf-tools : view and edit PDFs
;;
;; https://github.com/vedang/pdf-tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I had to do this to get installation to work:
;; https://github.com/politza/pdf-tools/issues/480#issuecomment-491440988

;; Later, this:
;;
;; export PKG_CONFIG_PATH=/opt/homebrew/lib/pkgconfig:$(brew --prefix libffi)/lib/pkgconfig/
;; /Users/candera/.emacs.d/elpa/pdf-tools-20240429.407/build/server/autobuild -i /Users/candera/.emacs.d/elpa/pdf-tools-20240429.407/

;; At some point, this also became necessary

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Make a per-user pkgconfig dir
;; mkdir -p ~/.pkgconfig

;; # Fill in a minimal bzip2.pc using Homebrew’s paths
;; BREW_BZ2_PREFIX="$(brew --prefix bzip2)"
;; cat > ~/.pkgconfig/bzip2.pc <<'EOF'
;; prefix=__PREFIX__
;; exec_prefix=${prefix}
;; libdir=${exec_prefix}/lib
;; includedir=${prefix}/include

;; Name: bzip2
;; Description: bzip2 compression library (Homebrew shim)
;; Version: 1.0.8
;; Libs: -L${libdir} -lbz2
;; Cflags: -I${includedir}
;; EOF

;; # Substitute the prefix
;; sed -i.bak "s#__PREFIX__#${BREW_BZ2_PREFIX}#g" ~/.pkgconfig/bzip2.pc
;; rm -f ~/.pkgconfig/bzip2.pc.bak

;; # Ensure pkg-config can see it (prepend is safest)
;; export PKG_CONFIG_PATH="$HOME/.pkgconfig:${PKG_CONFIG_PATH}"

;; # (Optional) also add common Homebrew pkgconfig dirs
;; export PKG_CONFIG_PATH="$(brew --prefix)/lib/pkgconfig:$(brew --prefix)/share/pkgconfig:${PKG_CONFIG_PATH}"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put that in e.g. /tmp/mkpc.sh then run `source /tmp/mkpc.sh` and then compile

(use-package pdf-tools
  :ensure t

  :config
  (pdf-tools-install t)

  :hook
  (pdf-tools-enabled . (lambda ()
			 (message "I got here")
			 (display-line-numbers-mode -1)
			 (iedit-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; wgrep-ag: writable ag results
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep-ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; casual-calc - menu system for calc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package casual-calc
  :ensure t

  :bind
  (:map calc-mode-map
	("?" . casual-calc-tmenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ultra-scroll: smooth mouse scrolling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prereq: evaluate `(package-vc-install '(ultra-scroll :vc-backend Git :url  "https://github.com/jdtsmith/ultra-scroll"))`

(use-package ultra-scroll
  ;:load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; term-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Built-in, no need for ensuring

(defun term-clear-to-beginning ()
  "Clears a term mode buffer from beginning of current line to top of buffer."
  (interactive)
  (save-excursion 
    (move-beginning-of-line nil)
    (push-mark)
    (beginning-of-buffer)
    (kill-region (point) (mark))))

;; Line mode
(define-key term-mode-map (kbd "C-c C-l") 'term-clear-to-beginning)

;; Char mode
(define-key term-raw-map (kbd "C-c C-l") 'term-clear-to-beginning)
(define-key term-raw-map (kbd "M-N") 'candera-next-window)
(define-key term-raw-map (kbd "M-P") 'candera-previous-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gcmh - Garbage Collection Magic Hack
;;   Runs more GC when idle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; fold-region
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fold-region ()
  "Hides region using outline-minor-mode."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode 1))
  (outline-flag-region (region-beginning) (region-end) t))

(defun unfold-region ()
  "Hides region using outline-minor-mode."
  (interactive)
  (unless outline-minor-mode
    (outline-minor-mode 1))
  (outline-flag-region (region-beginning) (region-end) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; diff-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix some keys
(define-key diff-mode-map (kbd "M-N") 'candera-next-window)
(define-key diff-mode-map (kbd "M-P") 'candera-previous-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand-region : expands region around point by incrementally larger
;; units of text
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure t

  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Aider.el - AI assistive coding
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aider
  :ensure t
  :config
  ;; For latest claude sonnet model
  ;; (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
  ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
  ;; Or chatgpt model
  (setq aider-args '("--model" "o4-mini" "--no-auto-commits"))
  (setenv "OPENAI_API_KEY" (read-gpg-file-to-string "~/.config/openapi-api-key.asc"))
  ;; Or gemini model
  ;; (setq aider-args '("--model" "gemini-exp"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; copilot.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :bind
  (:map copilot-mode-map
	("C-c TAB" . copilot-complete)
	("C-c c" . copilot-accept-completion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ace-jump-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "C-c C-j") 'ace-jump-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tramp-rclone
;;
;; Use rclone to open remote files like on S3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Run deferred setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (fboundp 'deferred-local-setup)
  (deferred-local-setup))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;
;; ; Varibles set by "customize" wind up here
;; ;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (if (functionp 'custom-set-variables-local)
;;     (custom-set-variables-local))
