;;; flexwiki-mode-el -- Major mode for editing FlexWiki files

;; Author: Scott Andrew Borton <scott@pp.htv.fi>
;; Created: 25 Sep 2000
;; Keywords: FlexWiki major-mode

;; Copyright (C) 2000, 2003 Scott Andrew Borton <scott@pp.htv.fi>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;; 
;; This mode is based on an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:

;; For the enormously useful word-at-point and thing-at-point-looking-at
(require 'thingatpt) 

;; For completions
(require 'pcomplete)

;; Set up completion
(defun flexwiki-setup-pcomplete ()
  (interactive) 
  "Initializes flexwiki to be able to use pcomplete completions"
  (set (make-variable-buffer-local 'pcomplete-default-completion-function)
       'flexwiki-wikiword-completions)
  (set (make-variable-buffer-local 'pcomplete-command-completion-function)
       'flexwiki-wikiword-completions)
  (set (make-variable-buffer-local 'pcomplete-parse-arguments-function)
       'flexwiki-current-word))

(defun flexwiki-wikiword-completions ()
  "Return a list of possible completions names for this buffer."
  (while 
      (pcomplete-here
       (mapcar 'flexwiki-extract-wikiword-from-filename
	       (pcomplete-entries ".*\\.wiki$" nil )
	       )
       )
    )
  )

(defun flexwiki-extract-wikiword-from-filename (filename)
  (interactive)
  "Extracts a wikiword like HomePage from a filename like HomePage.wiki"
  (save-excursion
    (save-match-data
      (substring filename (string-match "\\(.*\\)\\.wiki$" filename) (match-end 1))
      )
    )
  )

(defun flexwiki-current-word ()
  (let ((end (point)))
    (save-restriction
      (save-excursion
	(skip-chars-backward "^\\[ \t\n")
	(narrow-to-region (point) end))
      (pcomplete-parse-buffer-arguments)
      )
    )
  )

(defvar flexwiki-mode-hook nil)
(defvar flexwiki-mode-map
  (let ((map (make-keymap)))
    (define-key map [(control return)] 'flexwiki-follow-link)
    (define-key map [C-S-right] 'pcomplete)
    (define-key map [?	] 'self-insert-command)
    map)
  "Keymap for FlexWiki major mode")

(add-to-list 'auto-mode-alist '("\\.wiki$" . flexwiki-mode))

(defface flexwiki-wikiword-face
  `((t (:foreground "blue")))
  "The face in which a wikiword will appear")

(defface flexwiki-heading-1-face
  `((t (:height 1.5)))
  "The face in which heading level 1 (! Heading) text will appear")

(defface flexwiki-heading-2-face
  `((t (:height 1.3)))
  "The face in which heading level 2 (!! Heading) text will appear")

(defface flexwiki-heading-3-face
  `((t (:height 1.1)))
  "The face in which heading level 3 (!!! Heading) text will appear")

(defface flexwiki-strikethrough-face
  `((t (:strike-through t)))
  "The face in which strikethrough (-strikethrough text-) text will appear")

(defface flexwiki-bold-face
  `((t (:bold nil)))
  "The face in which bold (*bold text*) text will appear")

(defface flexwiki-italic-face
  `((t (:italic t)))
  "The face in which italic (_italic text_) text will appear")

(defface flexwiki-underline-face
  `((t (:underline t)))
  "The face in which underlined (+underlined text+) text will appear")

(defface flexwiki-property-face
  `((t (:foreground "green3")))
  "The face in which properties (name: value) text will appear")

(defface flexwiki-question-face
  `((t (:foreground "red2")))
  "The face in which questions (Q: question) text will appear")

(defface flexwiki-todo-face
  `((t (:foreground "dark violet")))
  "The face in which todo items (TODO: value) text will appear")

(defface flexwiki-done-face
  `((t (:foreground "dim grey")))
  "The face in which done items (Done: value) text will appear")
    
;; Needed to overcome a weird emacs bug
(defvar flexwiki-wikiword-face 'flexwiki-wikiword-face)
(defvar flexwiki-heading-1-face 'flexwiki-heading-1-face)
(defvar flexwiki-heading-2-face 'flexwiki-heading-2-face)
(defvar flexwiki-heading-3-face 'flexwiki-heading-3-face)
(defvar flexwiki-strikethrough-face 'flexwiki-strikethrough-face)
(defvar flexwiki-bold-face 'flexwiki-bold-face)
(defvar flexwiki-italic-face 'flexwiki-italic-face)
(defvar flexwiki-underline-face 'flexwiki-underline-face)
(defvar flexwiki-property-face 'flexwiki-property-face)
(defvar flexwiki-question-face 'flexwiki-question-face)
(defvar flexwiki-todo-face 'flexwiki-todo-face)
(defvar flexwiki-done-face 'flexwiki-done-face)

;; (defvar flexwiki-highlight-regexp "\\<_?[A-Z][a-z]+[A-Z][A-Z0-9a-z]*\\>" 
;;   "Defines what a WikiWord looks like")

;; The regexp that defines a WikiWord
(defconst flexwiki-wikiword-regexp
  "\\<_?[A-Z][a-z]+[A-Z][A-Z0-9a-z]*\\>")

;; The order matters here - put the patterns with more precedence first
(defconst flexwiki-font-lock-keywords-1
  (list
   '("^!!!.*" . flexwiki-heading-3-face)
   '("^!!.*" . flexwiki-heading-2-face)
   '("^!.*" . flexwiki-heading-1-face)
   '("\\W-\\(\\w.*\\w\\)-\\W" 1 flexwiki-strikethrough-face)
   '("\\*\\(.*\\)\\*" 1 flexwiki-bold-face)
   '("_.*_" . flexwiki-italic-face)
   '("\\+\\(.*\\)\\+" 1 flexwiki-underline-face)
   '("^Q:.*" . flexwiki-question-face)
   '("^TODO:.*" . flexwiki-todo-face)
   '("^Done:.*" . flexwiki-done-face)
   '("^\\w+:.*" . flexwiki-property-face)
   `(,flexwiki-wikiword-regexp . flexwiki-wikiword-face)
   )
  "Minimal highlighting expressions for FlexWiki mode.")

(defvar flexwiki-font-lock-keywords flexwiki-font-lock-keywords-1
  "Default highlighting expressions for FlexWiki mode.")

(defvar flexwiki-mode-syntax-table
  (let ((flexwiki-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" flexwiki-mode-syntax-table)
	
	flexwiki-mode-syntax-table)
  "Syntax table for flexwiki-mode")

(defun flexwiki-font-lock-setup ()
  (interactive)
  "Sets up FlexWiki font locking"
  (set (make-local-variable 'font-lock-defaults) 
       `('flexwiki-font-lock-keywords       ; Variable holding pattern-based highlighting rules
	 t        ; turn off string/comment highlighting
	 nil        ; Case-sensitive highlighting
	 nil        ; SYNTAX-ALIST: reclassify the character syntax of certain things
	 'beginning-of-line ; Syntactic regions don't cross lines
	 )
       )
  )

(defun flexwiki-follow-link ()
  (interactive)
  "Follows the WikiWord at point if there is one"
  (if (flexwiki-wikiword-at-point-p)
      (let ((filename (concat (word-at-point) ".wiki")))
	(let ((buffer (find-buffer-visiting filename))
	      (file-exists (file-exists-p filename)))
	  (if buffer 
	      (switch-to-buffer buffer)
	    (progn (find-file filename)
		   (if (not file-exists)
		       (write-file (buffer-file-name))
		     )
		   )
	    )
	  )
	)
    (message "Point is not on a WikiWord")
    )
  )

(defun flexwiki-wikiword-at-point-p ()
  (interactive)
  "True if point is currently positioned on a WikiWord"
  (save-excursion 
    (save-match-data
      (let ((case-fold-search nil))
	(string-match flexwiki-wikiword-regexp (word-at-point))
	)
      )
    )
  )

(defun flexwiki-wikiword-at-point-print ()
  (interactive)
  "True if point is currently positioned on a WikiWord"
  (save-excursion 
    (save-match-data
      (if (thing-at-point-looking-at flexwiki-wikiword-regexp)
	  "yes"
	"no"
	)
      )
    )
  )

(defvar flexwiki-tab-width 4 "The width of a tab character in flexwiki-mode")

;; (defun flexwiki-generate-tab-list (width)
;;   (let ((n 100) (tb 0) flexwiki-tab-stops)
;;     (while (> n 0)
;;       (progn 
;; 	(setq tb (+ tb width))
;; 	(setq n (- n 1))
;; 	(setq flexwiki-tab-stops (append flexwiki-tab-stops `(,tb)))
;; 	)
;;       )
;;     flexwiki-tab-stops
;;     )
;;   )    

;; (defun flexwiki-set-tab-size (&optional width)
;;   (interactive)
;;   "Set the width of a tab to be `width' if the argument is present, and flexwiki-tab-width otherwise"
;;   (let ((make-variable-buffer-local tab-stop-list))
;;     (setq tab-stop-list 
;; 	  (if width
;; 	      (flexwiki-generate-tab-list width)
;; 	    (flexwiki-generate-tab-list flexwiki-tab-width)
;; 	    )
;; 	  )
;;     )
;;   )
      

;; (defun flexwiki-font-lock-setup ()
;;   (interactive)
;;   "Sets up FlexWiki font locking"
;;   (set (make-local-variable 'font-lock-defaults) 
;;        `('(flexwiki-font-lock-keywords)       ; Variable holding pattern-based highlighting rules
;; 	 t        ; turn off string/comment highlighting
;; 	 nil        ; Case-sensitive highlighting
;; 	 nil        ; SYNTAX-ALIST: reclassify the character syntax of certain things
;; 	 'beginning-of-line ; Syntactic regions don't cross lines
;; 	 (font-lock-fontify-region-function . flexwiki-fontify-region) ; I have a function that fontifies
;; 	 (font-lock-unfontify-region-function. flexwiki-unfontify-region) ; and one that unfontifies
;; 	 )
;;        )
;;   )

;; (defun flexwiki-fontify-buffer ()
;;   "Re-highlight the entire Wiki buffer."
;;   (interactive)
;;   (flexwiki-fontify-region (point-min) (point-max) t))

;; (defvar flexwiki-highlight-vector nil "TODO: turn this into something")
;; (defvar flexwiki-highlight-buffer-hook nil "TODO: Implement")

;; (defun flexwiki-fontify-region (beg end &optional verbose)
;;   "Apply highlighting according to `flexwiki-highlight-markup'.
;; Note that this function should NOT change the buffer, nor should any
;; of the functions listed in `flexwiki-highlight-markup'."
;;   (let ((buffer-undo-list t)
;; 	(inhibit-read-only t)
;; 	(inhibit-point-motion-hooks t)
;; 	(inhibit-modification-hooks t)
;; 	(modified-p (buffer-modified-p))
;; 	deactivate-mark)
;;     (unwind-protect
;; 	(save-excursion
;; 	  (save-restriction
;; 	    (widen)
;; 	    ;; check to see if we should expand the beg/end area for
;; 	    ;; proper multiline matches
;; 	    (when (and font-lock-multiline
;; 		       (> beg (point-min))
;; 		       (get-text-property (1- beg) 'font-lock-multiline))
;; 	      ;; We are just after or in a multiline match.
;; 	      (setq beg (or (previous-single-property-change
;; 			     beg 'font-lock-multiline)
;; 			    (point-min)))
;; 	      (goto-char beg)
;; 	      (setq beg (line-beginning-position)))
;; 	    (when font-lock-multiline
;; 	      (setq end (or (text-property-any end (point-max)
;; 					       'font-lock-multiline nil)
;; 			    (point-max))))
;; 	    (goto-char end)
;; 	    (setq end (line-beginning-position 2))
;; 	    ;; Undo any fontification in the area.
;; 	    (font-lock-unfontify-region beg end)
;; 	    ;; And apply fontification based on `flexwiki-highlight-markup'
;; 	    (let ((len (float (- end beg)))
;; 		  (case-fold-search nil))
;; 	      (goto-char beg)
;; 	      (while
;; 		  (and (< (point) end)
;; 		       (re-search-forward flexwiki-highlight-regexp end t))
;; 		(if verbose
;; 		    (message "Highlighting buffer...%d%%"
;; 			     (* (/ (float (- (point) beg)) len) 100)))
;; 		(funcall (aref flexwiki-highlight-vector
;; 			       (char-after (match-beginning 0)))))
;; 	      (run-hook-with-args 'flexwiki-highlight-buffer-hook
;; 				  beg end verbose)
;; 	      (if verbose (message "Highlighting buffer...done")))))
;;       (set-buffer-modified-p modified-p))))

;; (defun flexwiki-unfontify-region (begin end &optional verbose)
;;   "Remove all visual highlights in the buffer (except font-lock)."
;;   (let ((buffer-undo-list t)
;; 	(inhibit-read-only t)
;; 	(inhibit-point-motion-hooks t)
;; 	(inhibit-modification-hooks t)
;; 	(modified-p (buffer-modified-p))
;; 	deactivate-mark)
;;     (unwind-protect
;; 	(remove-text-properties
;; 	 begin end '(face nil font-lock-multiline nil
;; 			  invisible nil intangible nil display nil
;; 			  mouse-face nil keymap nil help-echo nil))
;;       (set-buffer-modified-p modified-p))))


(defun flexwiki-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map flexwiki-mode-map)
  (set-syntax-table flexwiki-mode-syntax-table)
  ;; Set up font-lock
  (flexwiki-font-lock-setup)
  ;; Set up completion
  (flexwiki-setup-pcomplete)
  ;; Register our indentation function
  ;;(set (make-local-variable 'indent-line-function) 'flexwiki-indent-line)  
  (setq major-mode 'flexwiki-mode)
  (setq mode-name "FlexWiki")

  ;; Set it up so we always use tabs, not auto-indent
  (make-local-variable 'tab-always-indent)
  (setq tab-always-indent nil)

  ;; Make tabs four characters instead of 8
  (setq tab-width flexwiki-tab-width)

  ;; Run any installed hooks
  (run-hooks 'flexwiki-mode-hook))

(provide 'flexwiki)

;;; flexwiki-mode.el ends here




