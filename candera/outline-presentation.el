;; Enable narrowing, which is disabled by default
(put 'narrow-to-region 'disabled nil)

(defvar outline-presentation-mode nil
  "Outline presentation minor mode.")

(defvar outline-presentation-mode-map nil
  "Keymap for outline presentation mode")

(unless outline-presentation-mode-map
  (setq outline-presentation-mode-map (make-sparse-keymap))
  (define-key outline-presentation-mode-map "\M-n" 'outline-presentation-next)
  (define-key outline-presentation-mode-map "\M-p" 'outline-presentation-previous)
  (define-key outline-presentation-mode-map "\M-e" 'outline-presentation-mode-end)
  (define-key outline-presentation-mode-map [(shift f5)] 'outline-presentation-mode-off))

(defun outline-presentation-mode (&optional enable)
  "With no arguments, toggles outline-presentation minor mode.
  With a positive argument, turns outline-presentation minor mode
  on. With a negative argument, turns outline-presentation minor
  mode off."
  (interactive "P")
  (setq outline-presentation-mode 
	(if (null enable)
	    (not outline-presentation-mode)
	    (> (prefix-numeric-value enable) 0)))
  (if outline-presentation-mode
      (progn 
	(outline-presentation-start)
	(if (not (assq 'outline-presentation-mode minor-mode-alist))
	    (setq minor-mode-alist
		  (cons '(outline-presentation-mode " Presentation")
			minor-mode-alist)))
	(use-local-map outline-presentation-mode-map))
      (progn
	(use-local-map nil)
	(outline-presentation-end))))


(defun outline-presentation-mode-on ()
  "Turns on outline-presentation-mode"
  (interactive)

  (outline-presentation-mode 1))

(defun outline-presentation-mode-off ()
  "Turns off outline-presentation-mode"
  (interactive)
  (outline-presentation-mode -1))

(defun outline-presentation-start ()
  "Begin the presentation by making only the current node visible"
  (outline-mode) 
  (outline-back-to-heading)
  (let ((start (point)))
    (outline-next-heading)
    (narrow-to-region start (point)))
  (outline-back-to-heading t)
  (use-local-map outline-presentation-mode-map))

(defun outline-presentation-end ()
  "End the presentation"
  (widen))

(defun outline-presentation-next ()
  "Makes the next outline node the only visible node"
  (interactive)
  (widen)
  (outline-back-to-heading t)
  (outline-next-heading)
  (let ((start (point)))
    (outline-next-heading)
    (narrow-to-region start (point)))
  (outline-back-to-heading t))

(defun outline-presentation-previous ()
  "Make the previous outline node the only visible node"
  (interactive)
  (widen)
  (outline-back-to-heading t)
  (let ((start (point)))
    (outline-previous-heading)
    (narrow-to-region start (point)))
  (outline-back-to-heading t))


  