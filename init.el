(defun load-file-if-exists (path)
  (if (file-exists-p path)
      (load-file path)))

(defmacro comment (&rest body)
       "Comment out one or more s-expressions."
         nil)

;; Add the following to your init file to have packages installed by
;; Homebrew added to your load-path:
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-archives
 (append package-archives
         '(("melpa" . "http://melpa.org/packages/")
           ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
           ("org" . "http://orgmode.org/elpa/"))))
(package-initialize)

(dolist (package '(clojure-mode magit cider
                                smex ido-vertical-mode gherkin-mode
                                command-log-mode auto-complete
                                expand-region undo-tree haml-mode
                                csv-mode arduino-mode
                                inf-clojure csharp-mode yaml-mode paredit
                                paganini-theme))
  (unless (package-installed-p package)
    (package-install package)))

;; This would be great if it didn't just cause cider to completely disappear
;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Someone broke org-mobile. We have to load from a fixed copy.
;; (setq load-path (append '("~/.emacs.d/custom/org-mode/lisp"
;;                           "~/.emacs.d/custom/org-mode/contrib/lisp")
;;                         load-path))

;; Fix garbage collection so that it doesn't happen when the
;; minibuffer is open. See
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load nxhtml-mode (with MuMaMo)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load "~/.emacs.d/custom/nxhtml/autostart.el")


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stuff we want to load right away
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Save some buffers whenever emacs is idle
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change the way emacs handles buffer
;; names for files with the same name.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets various miscellaneous settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display line numbers
(global-display-line-numbers-mode t)

;; Enable CUA-style undo, cut, copy, paste, and selection
;;(cua-mode t)

;; For the love of all that is holy, unify the emacs kill ring and the
;; clipboard
(setq x-select-enable-clipboard t)

;; And don't allow tabs to be inserted
(setq-default indent-tabs-mode nil)

;; Enable ido-mode
(ido-mode 1)

;; And add a space after the line number in text-only terminals
;; (unless window-system (setq linum-format "%d "))

;; Enable line highlighting
(global-hl-line-mode 1)

(defun scroll-other-window-up-one ()
  "Scrolls other window towards top of buffer by one line"
  (interactive)
  (scroll-other-window-down 1))

(defun scroll-other-window-down-one ()
  "Scrolls other window towards bottom of buffer by one line"
  (interactive)
  (scroll-other-window-down -1))

(defun scroll-window-up-one ()
  "Scrolls other window towards top of buffer by one line"
  (interactive)
  (scroll-down 1))

(defun scroll-window-down-one ()
  "Scrolls other window towards bottom of buffer by one line"
  (interactive)
  (scroll-down -1))

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

(global-set-key (kbd "C-x 4 C-c") 'center-window-horizontally)
(global-set-key (kbd "C-x 4 C-r") 'restore-former-window-configuration)
(global-set-key (kbd "C-x 4 1") 'temporarily-display-one-window)
(global-set-key (kbd "C-x 4 2") 'temporarily-display-two-windows)
(global-set-key (kbd "C-x 4 3") 'temporarily-display-three-windows)
(global-set-key (kbd "C-x 4 4") 'temporarily-display-four-windows)
(global-set-key (kbd "M-N") 'other-window)
(global-set-key (kbd "M-P") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

;; Aliases for super-keys I confuse with the meta equivalent.
(global-set-key (kbd "s-N") 'other-window)
(global-set-key (kbd "s-y") 'yank-pop)
(global-set-key (kbd "s-b") 'backward-word)

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
(while  (
< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
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

(defun google-word-at-point ()
  "Opens a browser for the word at point on google.co"
  (interactive)
  (browse-url (format "https://www.google.com/search?q=%s" (word-at-point))))

(set-bar-cursor)

;; A single space ends a sentence
(setq sentence-end-double-space nil)

;; Use forward slashes between directory elements
;; TODO: This symbol has been deprecated. Change if there's a problem
(setq directory-sep-char ?\/)

;; Don't blink the cursor
(setq blink-cursor nil)

;; Move the mouse pointer out of the way when the cursor is near it
(mouse-avoidance-mode 'cat-and-mouse)

;; Turn off the menu bar and the tool bar, since I never use them.
;; Except under OS X, where they don't take up space
(unless (eq system-type 'darwin) (menu-bar-mode -1))

;; On some machines, tool-bar-mode is not bound, and it causes
;; initialization to bomb.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Set up some keybindings that I like
(global-set-key (quote [C-M-down]) 'scroll-other-window-down-one)
(global-set-key (quote [C-M-up])   'scroll-other-window-up-one)
(global-set-key (quote [C-up]) 'scroll-window-up-one)
(global-set-key (quote [C-down]) 'scroll-window-down-one)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (quote [C-tab]) 'other-window)

(global-set-key [f12] 'eshell)
;;(global-set-key [f11] (lambda () (interactive) (switch-to-buffer "*nrepl*")))
(global-set-key [f4] 'call-last-kbd-macro)
(global-set-key [f8] 'next-error)
(column-number-mode ())

(setq default-major-mode 'text-mode)
(setq display-time-day-and-date 'true)
;; display-time has the wonderful effect of causing my emacs to
;; totally hang for the first five seconds of every minute on windows.
;;(display-time)

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

(add-hook 'javascript-mode-hook 'turn-off-flyspell)
(add-hook 'emacs-lisp-mode-hook 'turn-off-flyspell)
(add-hook 'ruby-mode-hook 'turn-off-flyspell)

(defun inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (interactive)
  (let ((result (nth 4 (syntax-ppss))))
    (message "%s" result)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calendar customization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 38.8526° N, 77.3044° W
(setq calendar-latitude 38.9)
(setq calendar-longitude -77.3)
(setq calendar-location-name "Fairfax, VA")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Whitespace cleanup and display
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defface extra-whitespace-face
   '((t (:background "pale green")))
   "Used for tabs and such.")

(defvar extra-whitespace-keywords
  '(("\t" . 'extra-whitespace-face)))

(defun setup-highlight-whitespace ()
  (font-lock-add-keywords nil extra-whitespace-keywords)
  (setq show-trailing-whitespace t))

(defun stop-highlighting-whitespace ()
  (interactive)
  (font-lock-remove-keywords nil extra-whitespace-keywords)
  (setq show-trailing-whitespace nil))

(add-hook 'emacs-lisp-mode-hook 'setup-highlight-whitespace)
(add-hook 'text-mode-hook 'setup-highlight-whitespace)
(add-hook 'lisp-mode-hook 'setup-highlight-whitespace)
(add-hook 'ruby-mode 'setup-highlight-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set M-. to do something useful
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ctags command: ctags -R -e . --language-force=Clojure

(global-set-key (kbd "M-.") 'projectile-find-tag)
;; (global-set-key (kbd "M-.") 'xref-find-definitions)
;;(global-set-key (kbd "M-.") 'imenu)
;; (global-set-key (kbd "M-,") 'dumb-jump-back)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set M-' to do completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-'") 'completion-at-point)

;; (add-hook 'slime-mode-hook
;;           (lambda ()
;;             (define-key slime-mode-map (kbd "M-'") 'slime-complete-symbol)
;;             (define-key slime-mode-map (kbd "C-c M-q") nil)))

;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;;             (define-key slime-repl-mode-map (kbd "M-'") 'slime-complete-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up emacs-lisp-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (string= "raspberrypi" system-name)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Work around a bug in Ubuntu 10.10
(setq flyspell-issue-welcome-flag nil)

;; Turn on font-lock by default in all modes
(global-font-lock-mode t)

;; Highlight region between point and mark
(transient-mark-mode t)

(defun insert-new-line-above ()
  "Insert a new, blank line above the point"
  (interactive)
  (progn
    (move-to-column 0)
    (newline)
    (previous-line 1)
    (indent-according-to-mode)))

(global-set-key [\C-return] 'insert-new-line-above)

(defun set-big-font ()
  "sets the font to something readable from more than 3 inches away"
  (interactive)
  (modify-frame-parameters
   nil
   '( (font . "-outline-Courier-bold-r-normal-normal-19-142-96-96-c-110-iso10646-1"))))

(defun set-small-font ()
  "sets the font to something readable from more than 3 inches away"
  (interactive)
  (modify-frame-parameters
   nil
   '( (font . "-outline-Courier-bold-r-normal-normal-12-142-96-96-c-110-iso10646-1"))))

(defun set-default-font-size (height)
  "Sets the default font size to `height`."
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height height))

(defun set-default-frame-properties ()
  "Sets the frame properties back to the defaults"
  (interactive)
  (modify-frame-parameters
   nil
   default-frame-alist))

(defun insert-current-time ()
  "Inserts the current time at point"
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

(global-set-key "\C-c\C-t" 'insert-current-time)

;; Turn on parathesis highlighting in .el files
;(add-hook 'emacs-lisp-mode-hook
;         (lambda () (highlight-parentheses-mode 1)))


;; Tell emacs to wrap lines in vertically split windows
;; Much as I would like to leave this set to nil, it seems to cause all sorts of problems.
;;(setq truncate-partial-width-windows nil)
(setq truncate-lines t)

;; Except in inferior-lisp, where it screws things up
;; (add-hook 'inferior-lisp-mode-hook
;;           (lambda ()
;;              (setq truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up ido
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Make ido-mode list things vertically
;; (setq ido-decorations
;;       (quote
;;        ("\n-> "           ; Opening bracket around prospect list
;;         ""                ; Closing bracket around prospect list
;;         "\n   "           ; separator between prospects
;;         "\n   ..."        ; appears at end of truncated list of prospects
;;         "["               ; opening bracket around common match string
;;         "]"               ; closing bracket around common match string
;;         " [No match]"     ; displayed when there is no match
;;         " [Matched]"      ; displayed if there is a single match
;;         " [Not readable]" ; current diretory is not readable
;;         " [Too big]"      ; directory too big
;;         " [Confirm]")))   ; confirm creation of new file or buffer

;; ;; And let us use standard navagation keys that make sense vertically
;; (add-hook 'ido-setup-hook
;;           (lambda ()
;;             (define-key ido-completion-map [down] 'ido-next-match)
;;             (define-key ido-completion-map [up] 'ido-prev-match)
;;             (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;             (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;; ;; Make ido stop prompting me about whether I want to create a new buffer
;; (setq ido-create-new-buffer 'always)

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up dired-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dired-insert-this-directory-recursively (include-hidden)
  "Recursively insert the subdirectories of the current dired directory."
  (interactive "P")
  (dired-insert-subdir dired-directory (if (null include-hidden)
                                           "-lR"
                                         "-alR")))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "I") 'dired-insert-this-directory-recursively)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Associate javascript mode with .js files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Associate ruby mode with ruby files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("Rakefile$\\|Gemfile$\\|\\.rake$\\|Capfile$\\|\\.watchr$\\|Guardfile$\\|\\.ru$\\|\\.gemspec$" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Associate yaml-mode with yml files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set M-. to be imenu where SLIME isn't available
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'ruby-mode-hook (lambda () (define-key ruby-mode-map (kbd "M-.") 'imenu)))
;; (define-key emacs-lisp-mode-map (kbd "M-.") 'imenu)
;; (add-hook 'javascript-mode-hook (lambda () (define-key js-mode-map (kbd "M-.") 'imenu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up simple refactoring support
;; (only symbol renaming at this point)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'refactor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up lisp-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support paredit for better paren
;;; editing in Lisp mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
(add-hook 'lisp-mode-hook '(lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook '(lambda () (paredit-mode +1)))

(show-paren-mode t)

;; (load "paredit.el")

;; (define-key paredit-mode-map (kbd "M-)")
;;   'paredit-close-parenthesis-and-newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for SLIME
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/custom/slime-2010-05-19/")  ; your SLIME directory
;; (add-to-list 'load-path "~/.emacs.d/custom/slime-2010-05-19/contrib")  ; your SLIME contrib directory


;; ;;(add-to-list 'load-path "~/emacs/slime-2008-11-03/")  ; your SLIME directory
;; ;;(add-to-list 'load-path "~/emacs/slime-2008-11-03/contrib")  ; your SLIME contrib directory
;; ;; (setq
;; ;;   ; inferior-lisp-program "C:/bin/clisp-2.45/clisp -K full"  ; your Lisp system
;; ;;   ;inferior-lisp-program "C:/bin/sbcl-1.0.14.22/sbcl --core C:/bin/sbcl-1.0.14.22/sbcl.core"  ; your Lisp system
;; ;;   slime-complete-symbol-function 'slime-fuzzy-complete-symbol  ; fuzzy symbol completion (requires slime-fuzzy from contrib)
;; ;;   ;slime-complete-symbol-function 'slime-complete-symbol  ; standard symbol completion
;; ;;   lisp-indent-function 'common-lisp-indent-function            ; How would you like to indent?
;; ;;  )

;; (require 'slime-autoloads)
;; (slime-setup '(slime-repl slime-editing-commands slime-fuzzy slime-presentations slime-scratch))
;; ;(slime-setup '(slime-fancy))

;; ;(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)  ; fuzzy symbol completion (requires slime-fuzzy from contrib)

;; (if (functionp 'slime-local-setup)
;;     (slime-local-setup))

;; ;; Turn off the annoying SLIME version mismatch message

;; (eval-after-load 'slime '(setq slime-protocol-version 'ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Autocomplete for SLIME
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'ac-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allow input to be sent to somewhere other than inferior-lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell-send-input (input &optional buf)
  "Send INPUT into the *shell* buffer (or `buf` if specified) and leave it visible."
  (save-selected-window
    (switch-to-buffer-other-window (or buf "*shell*"))
    (goto-char (point-max))
    (insert input)
    (comint-send-input)))

(defun defun-at-point ()
  "Return the text of the defun at point."
  (apply #'buffer-substring-no-properties
         (region-for-defun-at-point)))

(defun region-for-defun-at-point ()
  "Return the start and end position of defun at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (beginning-of-defun)
        (list (point) end)))))

(defun expression-preceding-point ()
  "Return the expression preceding point as a string."
  (buffer-substring-no-properties
   (save-excursion (backward-sexp) (point))
   (point)))

(defun shell-eval-last-expression ()
  "Send the expression preceding point to the *shell* buffer."
  (interactive)
  (shell-send-input (expression-preceding-point)))

(defun shell-eval-defun ()
  "Send the current toplevel expression to the *shell* buffer."
  (interactive)
  (shell-send-input (defun-at-point)))

(defun shell-eval-region ()
  "Send the contents of the region to the *shell* buffer."
  (interactive)
  (if (use-region-p)
    (shell-send-input (buffer-substring-no-properties
                       (region-beginning)
                       (region-end)))
    (error "The region is not active - nothing to evaluate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OMFG: cider requires org-mode, which will pull in the default
;; version built in to Emacs if I don't set it up before
;; clojure-mode/cider.
;;
;; Set up later version of org-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org :ensure t)

(require 'org)
(require 'org-install)

(define-key org-mode-map (kbd "H-g") 'counsel-org-goto)

(global-set-key (kbd "C-c a") 'org-agenda-view-mode-dispatch)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Bizarrely, org-clock defaults to showing the current year only
(setq org-clock-display-default-range 'untilnow)

(add-hook 'org-mode-hook (lambda ()
                           (turn-on-flyspell)
                           ;; I always type this instead of C-c C-t
                           (define-key org-mode-map (kbd "C-c t") 'org-todo)
                           (auto-revert-mode 1)
                           (add-to-list 'org-modules 'org-habit)
                           ;; Org files can get big and have lots of
                           ;; folded content. There's not much benefit
                           ;; in line numbers, and they slow down org
                           ;; noticably.
                           (linum-mode 0)
                           ;; Weird that I have to do this, but I
                           ;; can't figure out how to get habits
                           ;; turned on outside of the customization
                           ;; interface, which I prefer not to use.
                           (require 'org-habit)
                           ;; Automatically save org-mode buffers when
                           ;; idle. Important because I use Dropbox to
                           ;; sync them, and forgetting to save when
                           ;; switching computers means conflicts.
                           (add-to-list 'idle-save-buffer-list (current-buffer))))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            ;; I always type this instead of C-c C-t
            (define-key org-agenda-mode-map (kbd "C-c t") 'org-agenda-todo)))

;; This requests logging when going from TODO to INPROGRESS and from INPROGRESS to DONE
(setq org-todo-keywords (quote ((sequence "TODO(t!)" "INPROGRESS(i!)" "PAUSED(p@)" "BLOCKED(b@)" "DONE(d!)"))))

;; org-mode refuses to invoke org-indent-mode in emacs 23, claiming
;; that it might crash. So I set this variable, which gets me the same
;; effect.
(setq org-hide-leading-stars t)

;; Let me refile by path, and to deeper nesting
(setq org-refile-use-outline-path 'file)
;;(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets
      '((org-agenda-files . (:maxlevel . 5))))

;; Log into a drawer, which is nice
(setq org-log-into-drawer t)

;; Include things in the diary file
(setq org-agenda-include-diary t)

;; Make events sort by newest first in the agenda view
(setq org-agenda-sorting-strategy
      '((agenda priority-down timestamp-down habit-down time-up category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

;; Store captured notes in notes.org, and bind capture to C-c c
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "C-c c") 'org-capture)


;; Log time task was closed
(setq org-log-done t)

;; Turn off the annoying mouse highlight in agenda views
(add-hook 'org-finalize-agenda-hook
          (lambda () (remove-text-properties
                      (point-min) (point-max) '(mouse-face t))))

;; Set up for agendas and mobile org
(when (file-exists-p "~/Dropbox/org/")
  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/Dropbox/org/")
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/MobileOrg")
  ;; A file that lists which org files should be pulled into the agenda
  (setq org-agenda-files "~/Dropbox/org/agendas.org"))

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
heuristic. Namely, by task state: INPROGRESS, then BLOCKED, the
TODO, then nothing, then DONE. Within the non-done states, sort
by scheduled, or by deadline if not scheduled, with oldest dates
first. Within DONE, most-recently done first. Archived items are
always last."
  (format "%s/%s-%s/%s"
          (if (member "ARCHIVE" (org-get-tags)) "1" "0")
          (pcase (org-get-todo-state)
            ("INPROGRESS" 1)
            ("BLOCKED" 2)
            ("TODO" 3)
            (`nil 4)
            ("DONE" (format "5%20d" (let ((ct (org-entry-get (point) "CLOSED")))
                                      (if ct
                                          (org-time-difference ct "2038-01-01")
                                        1.0e23))))
            (otherwise 6))
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

;; Compute my own custom scores for habits

(defun org-candera-habit-penalty (days-since-last)
  (- (if days-since-last
         (if (< 1 days-since-last)
             (- (expt 2 days-since-last)
                2)
           0)
       0)))

(defun org-candera-habit-score (days today initial-value)
  (when days
    (let ((score-info
           (reduce (lambda (acc d)
                     (let* ((last-day (gethash :last-day acc))
                            (days-since-last (when last-day (- d last-day)))
                            (on-streak? (when days-since-last (= 1 days-since-last)))
                            (streak (if on-streak? (1+ (gethash :streak acc 0)) 1))
                            (streak-bonus (if on-streak?
                                              (if (zerop (mod streak 3))
                                                  (/ streak 3)
                                                0)
                                            0))
                            (score (gethash :score acc 0)))
                       (puthash :last-day d acc)
                       (puthash :streak streak acc)
                       (puthash :score (max 1 (+ (or score 0)
                                                 streak-bonus
                                                 1
                                                 (org-candera-habit-penalty days-since-last)))
                                acc)
                       acc))
                   days
                   :initial-value initial-value)))
      (let ((today-penalty (org-candera-habit-penalty (- (1+ today)
                                                         (first (last days))))))
        (puthash :score
                 (max 0 (+ (gethash :score score-info) today-penalty))
                 score-info)
        (unless (zerop today-penalty)
          (puthash :streak 0 score-info))
        score-info))))

(defun org-collect-dates-for-element ()
  "Gets all the dates for the element at point"
  (let* ((element (org-element-at-point))
         (start (org-element-property :begin element))
         (end (org-element-property :end element)))
    (org-get-all-dates start end nil nil t)))

(defun org-collect-dates (match)
  "Returns all the unique dates that appear in items that match MATCH"
  ;; TODO: Figure how to keep it from scanning both parents and
  ;; children, since that's redundant
  ;; TODO: Skipping archived items doesn't seem to work,
  ;; although skipping commented items does.
  (let* ((dates (apply #'append
                       (org-map-entries #'org-collect-dates-for-element
                                        match
                                        'file
                                        'archive
                                        'comment)))
         (uniques (cl-remove-duplicates dates))
         (sorted ))
    (cl-sort uniques #'<)))

(defun org-dblock-write:compute-habit-score (params)
  "Returns a 'score' for entries that match `match` (e.g. a tag)
  based on timestamps that appear in them.

  One point is given for each consecutive day that appears. A day
  without activity drops the score by (expt 2
  days-since-last-activity). Every third day of a streak, a bonus
  of (/ streak-length 3) is awarded.

  If not all data is recorded in the org file initially, initial
  values can be provided via :last-day, :initial-streak,
  and :initial-score params."
  (interactive)
  (let* ((last-day-param (plist-get params :last-day))
         (last-day (when last-day-param
                     (time-to-days (org-time-string-to-time last-day-param))))
         (initial-streak (plist-get params :initial-streak))
         (initial-score (plist-get params :initial-score))
         (initial-value (make-hash-table))
         (match (plist-get params :match)))
    (puthash :last-day last-day initial-value)
    (puthash :streak initial-streak initial-value)
    (puthash :score initial-score initial-value)
    (save-excursion
      (let* ((dates (org-collect-dates match))
             (score-info (org-candera-habit-score
                          (if last-day
                              (remove-if (lambda (d) (<= d last-day)) dates)
                            dates)
                          (time-to-days (current-time))
                          initial-value)))
        (insert
         (format "Score as of %s: %s\nStreak: %d"
                 (format-time-string "%Y-%m-%d" (current-time))
                 (if score-info (or (gethash :score score-info) "No score") "No score")
                 (if score-info (or (gethash :streak score-info) 0) 0)))))))

(defun candera:goal-achieved?
  (achieved?)
  (string-prefix-p achieved? "y" t))

(defun candera:streak-game-compute-elapsed
  (current-date prior-date current-achievement prior-achievement prior-elapsed)
  (let ((date-difference (floor
                          (/ (org-time-difference current-date prior-date)
                             (* 24 60 60.0)))))
    (if (candera:goal-achieved? current-achievement)
        (+ prior-elapsed date-difference)))
  )

;; I don't want to see days in cumulative durations, thanks
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up org-babel
;; Stolen from https://github.com/stuartsierra/dotfiles/blob/2ec5ab2a45c091d74c8e73d62683b15ddd8bd9c7/.emacs.d/local/init.el#L295
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)
(require 'cider)

;; Don't make me confirm evaluation every single time
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (shell . t)
   (dot . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require clojure-mode to load and associate it to all .clj files.

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.hl$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("\\.cljc$" . clojurec-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.dtm$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

;; (require 'dumb-jump)
;; (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljc"))
;; (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs"))
;; (add-to-list 'dumb-jump-language-file-exts '(:language "clojure" :ext "cljs.hl"))

;; Turn on paredit for clojure files
;(require 'clojure-paredit)
(setq clojure-enable-paredit t)

;; This gives us a way to turn off slime-mode via .dir-locals.el. Just
;; execute add-dir-local-variable to set clojure-mode-no-slime to t,
;; and after that slime-mode will be turned off in any clojure-mode
;; buffer that gets opened in that directory structure.
(defvar clojure-mode-no-slime nil)

;; We have to use hack-local-variables-hook, because apparently
;; clojure-mode-hook runs before the local variables are set.
(add-hook 'hack-local-variables-hook
          '(lambda () (when clojure-mode-no-slime
                        (message "Disabling slime-mode because clojure-mode-no-slime is set")
                        (slime-mode -1))))

;; Work around bug where inf-clojure-minor-mode sets
;; comint-input-sender too aggressively
(make-variable-buffer-local comint-input-sender)

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
                (eldoc-mode 1)
                (cider-mode 0)))
            nil
            t)
  ;; (highlight-parentheses-mode 1)
  ;; (unless (eq major-mode 'cider-repl-mode)
  ;;   (display-line-numbers-mode 1))
  (highlight-symbol-mode t)
  (paredit-mode 1)
  (hs-minor-mode 1)
  (auto-complete-mode 1)
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

;;(require 'clojure-test-mode)

;; Some indentation fixups for core.async
;;(put-clojure-indent 'go-loop 1)         ; Like 'let'

;; Same thing for core.typed
;;(put-clojure-indent 'doseq> 1)          ; Like 'let'
;;(put-clojure-indent 'for> 1)            ; Like 'let'

;; clojure-fill-docstring got changed rather radically in a newer
;; version of clojure-mode than the one I use. I prefer the one I
;; wrote, so I override it here. I also made a few changes, like
;; respecting markdown syntax to give me things like correctly
;; indenting bulleted lists.
(defun clojure-fill-docstring ()
  "Fill the definition that the point is on appropriate for Clojure.

Fills so that every paragraph has a minimum of two initial spaces,
with the exception of the first line.  Fill margins are taken from
paragraph start, so a paragraph that begins with four spaces will
remain indented by four spaces after refilling."
  (interactive)
  (if (and (fboundp 'paredit-in-string-p) paredit-mode)
      (unless (paredit-in-string-p)
        (error "Must be inside a string")))
  ;; Oddly, save-excursion doesn't do a good job of preserving point.
  ;; It's probably because we delete the string and then re-insert it.
  (let ((old-point (point)))
    (save-restriction
      (save-excursion
        (let* ((clojure-fill-column 70)
               (string-region (clojure-docstring-start+end-points))
               (string-start (1+ (car string-region)))
               (string-end (1- (cdr string-region)))
               (string (buffer-substring-no-properties string-start
                                                       string-end)))
          (delete-region string-start string-end)
          (insert
           (with-temp-buffer
             ;; Bah, this doesn't work, because it isn't idempotent.
             ;; To make it so, and to preserve correctly line flow for
             ;; things like bulleted lists, it looks like we might
             ;; have to heuristically detect that every non-blank line
             ;; starts with two spaces and remove them before trying
             ;; again. I think the fix might be to make
             ;; `markdown-adaptive-fill-function` aware of
             ;; `left-margin`.
             (insert string)
             (markdown-mode)
             (setq fill-column (- clojure-fill-column 2))
             (fill-region (point-min) (point-max))
             (goto-char (point-min))
             (replace-regexp "^" "  ")
             (delete-trailing-whitespace)
             (buffer-substring-no-properties (+ 2 (point-min)) (point-max)))))))
    (goto-char old-point)))

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
                   (contents (buffer-substring-no-properties (1+ start) (1- end))))
      (lexical-let* ((new-buffer (switch-to-buffer (make-temp-name "clojure-string")))))
      (local-set-key (kbd "C-c C-c") (lambda ()
                                       (interactive)
                                       (save-mark-and-excursion
                                         (lexical-let* ((contents (buffer-substring-no-properties
                                                                   (point-min)
                                                                   (point-max))))
                                           (switch-to-buffer orig)
                                           (kill-region (1+ start) (1- end))
                                           (goto-char (1+ start))
                                           (insert contents)))))
      (insert contents)
      (normal-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; inferior-lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'inf-lisp)
(require 'clojure-mode)
(require 'paredit)

;; Right now I only use inferior lisp for clojure, so we configure for that
(define-key clojure-mode-map (kbd "C-c C-c") 'lisp-eval-defun)

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (paredit-mode t)))

(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(define-key inferior-lisp-mode-map (kbd "C-c M-o") #'comint-clear-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cider
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cider)

;; Don't use on the Pi, due to excessive CPU
(unless (string= "raspberrypi" system-name)
  (add-hook 'cider-mode-hook
            (lambda ()
              (eldoc-mode)
              (cider-eldoc-setup)
              ;; Suppress some really stupid shit that cider is doing
              ;; around background colors. I think it's assuming
              ;; there's a theme. Or it could be the problem that when
              ;; cider loads, the background color is still light.
              ;; (setq cider-stacktrace-frames-background-color
              ;;       (cider-scale-background-color))
              )))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (paredit-mode 1)
            ;; For some reason this isn't defined correctly
            (define-key cider-repl-mode-map (kbd "{") #'paredit-open-curly)
            (define-key cider-repl-mode-map (kbd "}") #'paredit-close-curly)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elein-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'elein)

;; (defun lein-repl ()
;;   "Run 'lein repl' in an inferior-lisp."
;;   (interactive)
;;   (inferior-lisp "lein repl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up Craig's web-lookup utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up Craig's typing-speed-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/custom/candera/typing-speed.el")
;; (add-hook 'text-mode-hook (lambda ()
;;                             (turn-on-typing-speed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up Craig's view-visited-file-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'view-visited-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up Craig's outline-presentation-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on Craig's outline-presentation hacks.
(load "~/.emacs.d/custom/candera/outline-presentation.el")

(global-set-key (quote [f5]) 'outline-presentation-mode-on)
(global-set-key (quote [f6]) 'lisp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up Craig's journal functionality
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/custom/candera/journal.el")
(global-set-key (kbd "C-x y") 'find-yesterday-log-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sgml-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sgml-mode)

(setq auto-mode-alist
      (append '(
             ("\\.xml$" . sgml-mode)
             ("\\.build$" . sgml-mode)
             ("\\.config$" . sgml-mode)
             ("\\.xslt$" . sgml-mode)
             ("\\.xsl$" . sgml-mode)
             ("\\.xsd$" . sgml-mode)
             ) auto-mode-alist ))

(defun sgml-pretty-print-buffer ()
  "Format the entire buffer using sgml-pretty-print"
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))))

(add-hook 'sgml-mode-hook
          (lambda ()
            (define-key sgml-mode-map (kbd "C-c p") 'sgml-pretty-print-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Set up the command shell
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun my-shell-setup ()
;;   "For cmdproxy shell under Emacs 20"
;;   (setq w32-quote-process-args ?\")
;;   (make-variable-buffer-local 'comint-completion-addsuffix))

;; (setq shell-mode-hook 'my-shell-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This sets CVS to use plink, which SourceForge requires
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "CVS_RSH" "plink")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This sets up my FlexWiki mode
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.wiki$" . flexwiki-mode))
(require 'flexwiki)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Sets up outline-mode
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.outline$" . outline-mode))
(add-hook 'outline-mode-hook 'auto-fill-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sets up highlight-symbol mode
;;
;; http://nschum.de/src/emacs/highlight-symbol/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up markdown-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode "\\.md"
  :config
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up magit and magit-modes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up gist.el
;;
;; http://github.com/defunkt/gist.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'gist)

;; ;; Turn on longlines mode whenever we're in an edit server buffer
;; (add-hook 'edit-server-text-mode-hook
;;           '(lambda ()
;;              (longlines-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; diary setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq diary-file "~/Dropbox/diary")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up nXhtml mode
;;
;; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(load "~/.emacs.d/custom/nxhtml/autostart.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Change zap-to-char not to kill the char
;; it zaps to. Taken from
;; http://www.emacswiki.org/emacs/ZapUpToChar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactively evaluate SPARQL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'sparql-mode "sparql-mode.el"
     "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

(setq sparql-default-base-url "http://localhost:2020/metamodl_test/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up ERC
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(add-hook 'erc-insert-post-hook 'ding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; find-file-in-git-repo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Deprecating in favor of find-file-in-project
;; (require 'find-file-in-git-repo)
;; (global-set-key (kbd "C-x M-f") 'find-file-in-git-repo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; find-file-in-project
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'find-file-in-project)
;; (global-set-key (kbd "C-x M-f") 'find-file-in-project)
;; (setq ffip-patterns (append '("*.clj" "*.cljc" "*.cljs" "*.scss" ".css" "*.java" "*.dtm" "*.edn") ffip-patterns))
;; (setq ffip-limit 2048)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helm-projectile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-projectile
  :ensure t)

(require 'helm-projectile)
(global-set-key (kbd "C-x M-f") 'helm-projectile-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helm-ag
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Even though it breaks sometimes, it's still better than counsel-ag
;; because it uses a separate buffer.
(require 'helm-ag)
(global-set-key (kbd "C-x M-s") 'helm-do-ag-project-root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; align-cljlet, for aligning let forms
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'align-cljlet)
;; (define-key clojure-mode-map (kbd "C-c |") 'align-cljlet)

;; Deprecated - replaced by clojure-mode support
(define-key clojure-mode-map (kbd "C-c |") 'clojure-align)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SMEX
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smex)
(smex-initialize)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ivy, Counsel, Company
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :ensure t)

(require 'ivy)
(ivy-mode 1)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

(require 'counsel)

(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Don't make M-x match on beginning of string
(add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flx
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flx
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
  (setq projectile-switch-project-action 'projectile-commander))

;; (global-set-key (kbd "C-x M-f") 'counsel-projectile-find-file)
;;(global-set-key (kbd "C-x M-s") 'counsel-projectile-ag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand-region
;;   https://github.com/emacsmirror/expand-region.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-complete-mode
;;   https://github.com/auto-complete/auto-complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)
(ac-config-default)

;; Turn off automatic start of auto-complete
(setq ac-auto-start nil)

(add-hook 'auto-complete-mode-hook
          (lambda ()
            (local-set-key (kbd "M-/") 'auto-complete)
            (define-key ac-completing-map (kbd "C-n") 'ac-next)
            (define-key ac-completing-map (kbd "C-p") 'ac-previous)
            (define-key ac-completing-map (kbd "C-g") 'ac-stop)
            (define-key ac-completing-map (kbd "ESC") 'ac-stop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; command-log-mode
;;  https://github.com/lewang/command-log-mode.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'command-log-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; undo-tree-mode
;; http://www.dr-qubit.org/git/undo-tree.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eshell customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; haml-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'haml-mode)

(add-hook 'haml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (define-key haml-mode-map "\C-m" 'newline-and-indent)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; gherkin-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'gherkin-mode)
(add-to-list 'auto-mode-alist '("\\.gk$" . gherkin-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; csv-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; inf-clojure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'inf-clojure-mode-hook
          (lambda ()
            (paredit-mode 1)
            (eldoc-mode 1)))

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
  (interactive (lexical-let* ((dir (read-directory-name "Project directory: "
                                                        (locate-dominating-file
                                                          default-directory
                                                          ".git")
                                                        nil
                                                        t))
                              (_   (switch-to-buffer-other-window "*inf-clojure*"))
                              (_   (cd dir))
                              (_   (hack-dir-local-variables-non-file-buffer))
                              (dir-vars (rest
                                         (assoc 'clojure-mode
                                                (assoc-default dir
                                                               dir-locals-class-alist
                                                               #'(lambda (i k)
                                                                   (file-equal-p k (symbol-name i)))))))
                              (cmd (read-string "Run Clojure: "
                                                (assoc-default 'use-inf-clojure-program
                                                               dir-vars)))
                              (name (read-buffer "REPL buffer name: "
                                                 (assoc-default 'inf-clojure-buffer
                                                                dir-vars))))
                 (when (get-buffer name)
                    (when (y-or-n-p "REPL buffer exists. Delete existing?")
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

(defun inf-clojure-refresh ()
  "Runs `(refresh)` in the attached REPL."
  (interactive)
  (let ((cur (current-buffer)))
    (pop-to-buffer inf-clojure-buffer)
    (goto-char (point-max))
    (insert "(require 'clojure.tools.namespace.repl) (if-let [r (resolve 'user/reset)] (r) (clojure.tools.namespace.repl/refresh))")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Un-pork scrolling
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hoplon support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))

(add-hook 'clojure-mode-hook
          '(lambda ()
             ;; Hoplon functions and macros
             (dolist (pair '((page . 'defun)
                             (loop-tpl . 'defun)
                             (cell-let . 'defun)
                             (if-tpl . '1)
                             (for-tpl . '1)
                             (for-append . '1)
                             (keyed-for-tpl . '2)
                             (map-lens-tpl . '2)
                             (do-watch . '1)
                             (case-tpl . '1)
                             (cond-tpl . 'defun)
                             (formula-of . 'defun)
                             (GET . 'defun)
                             (PUT . 'defun)
                             (POST . 'defun)))
               (put-clojure-indent (car pair)
                                   (car (last pair))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flyspell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flyspell)

(defun flyspell-popup-correct-previous-word ()
  (interactive)
  ;; This hack brought to you courtesy of
  ;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'flyspell-auto-correct-word) #'flyspell-popup-correct))
    (flyspell-auto-correct-previous-word (point))))

(global-set-key (kbd "C-;") 'flyspell-popup-correct-previous-word)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-popup-correct-previous-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; restclient
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Not working yet - the execution is asynchronous
;; (defun org-babel-execute:restclient (body params)
;;   (save-mark-and-excursion
;;    (with-temp-buffer
;;      (insert body)
;;      (restclient-http-send-current nil t)
;;      (switch-to-buffer "*HTTP Response*")
;;      (buffer-substring (point-min) (point-max)))))

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; csharp-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lets me use tags to navigate csharp files
(require 'csharp-mode)
(require 'semantic/symref/grep)
(setq semantic-symref-filepattern-alist
      (append semantic-symref-filepattern-alist
              '((csharp-mode "*.cs" "*.CS"))))

(add-hook 'csharp-mode-hook
          (lambda ()
            ;; (display-line-numbers-mode 1)
            (setq c-basic-offset 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; java-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; sql-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sqlup-mode
  :ensure t)

(require 'sql)

(define-key sql-mode-map (kbd "C-M") 'newline)

(require 'sqlup-mode)

(add-hook 'sql-mode-hook
          (lambda ()
            (sqlup-mode 1)))

;; sql-eval-mode

(defvar sql-eval-mode-shell-buffer "")
(make-variable-buffer-local 'sql-eval-mode-shell-buffer)

(defvar sql-eval-mode-style :dosql)
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
          (if (eq sql-eval-mode-style :dosql)
              (sql-to-single-line sql)
            sql)))

(defun sql-eval-buffer-subset (buf beg end)
  "Send the text in the buffer from `beg` to `end` to SQL eval buffer `buf`"
  (save-excursion
    (save-match-data
      (goto-char beg)
      (let ((cur (point)))
        (while (< cur end)
          (goto-char cur)
          (lexical-let* ((next-go (re-search-forward "^GO$" nil t))
                         (block-end (min end (if next-go
                                                 (- next-go
                                                    (if (eq sql-eval-mode-style :dosql)
                                                        2
                                                      0))
                                               end)))
                         (sql (buffer-substring-no-properties cur block-end))
                         (prepped-sql (sql-eval-prep-input sql)))
            (shell-send-input prepped-sql buf)
            (when (eq sql-eval-mode-style :sqlcmd)
              (shell-send-input "GO" buf))
            (setq cur (if next-go (1+ next-go) block-end))))))))

(defun sql-eval-region (buffer)
  "Send the contents of the region to the *shell* buffer. Strips newlines from the string first.

With a prefix arg, prompts for the buffer to send to."
  (interactive "P")
  (let ((buf (if (and (null buffer) (not (string= "" sql-eval-mode-shell-buffer)))
                sql-eval-mode-shell-buffer
              (read-buffer "Buffer: " "staging" t))))
    (if (use-region-p)
        (sql-eval-buffer-subset buf
                                (region-beginning)
                                (region-end))
      (error "The region is not active - nothing to evaluate"))))

(defun sql-eval-defun (buffer)
  "Send the text surrounding point (to the nearest blank line) to the *shell* buffer.

With a prefix arg, prompts for the buffer to send to."
  (interactive "P")
  (let ((buf (if (and (null buffer) (not (string= "" sql-eval-mode-shell-buffer)))
                 sql-eval-mode-shell-buffer
               (read-buffer "Buffer: " "staging" t))))
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

(defvar sql-eval-interpreter "dosql -i --tall")

(defvar sql-eval-mode-map (make-keymap))
(define-key sql-eval-mode-map (kbd "C-c e") 'sql-eval-region)
(define-key sql-eval-mode-map (kbd "C-c C-r") 'sql-eval-region)
(define-key sql-eval-mode-map (kbd "C-M-x") 'sql-eval-defun)
(define-key sql-eval-mode-map (kbd "C-c C-c") 'sql-eval-defun)
(define-key sql-eval-mode-map (kbd "C-c C-b") 'sql-eval-set-buffer)

(define-minor-mode sql-eval-mode
  "A minor mode for evaluating SQL statements by sending them to a comint buffer."
  :lighter (:eval (sql-eval-mode-lighter)))

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-eval-mode 1)))

(defun sql-eval-start-process (interpreter &optional name envs)
  "Starts a shell that actually works with comint mode. Defaults
to `sql-eval-interpreter` for interpreter."
  (interactive "P")
  ;; We need process-connection-type to be nil, or it chokes whenever
  ;; the input exceeds a certain length.
  (let* ((interpreter (if (null interpreter)
                          sql-eval-interpreter
                        (read-string "Interpreter: ")))
         (name (or name (read-buffer "Buffer: " nil nil)))
         (envs (or envs (read-string "Envs: ")))
         (process-connection-type nil)
         (temp-name (symbol-name (gensym)))
         (process (make-comint temp-name "bash" nil "-i"))
         (starred-name (concat "*" temp-name "*")))
    (switch-to-buffer-other-window starred-name)
    (rename-buffer name)
    ;; Somehow inf-clojure is setting this variable in my SQL Eval
    ;; buffers, which is screwing things up royally. Clobber it back.
    (make-variable-buffer-local 'comint-input-sender)
    (setq comint-input-sender 'comint-simple-send)
    ;; This is a hack to get gpg-agent to have the keys we need. I
    ;; haven't been able to figure out how to get zerkenv to do it
    ;; correctly on its own when run under emacs
    ;; (epa-decrypt-file "~/.adzerk-aws-creds.asc" "/dev/null")
    (process-send-string process "zerk\n")
    (process-send-string process (concat  "zerkload " envs "\n"))
    ;;(process-send-string process (concat "zerkenv --yes --source " envs "\n"))
    (process-send-string process (concat interpreter "\n"))))

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
         (sql-product-val (alist-get :sql-product arguments))
         (style (alist-get :sql-eval-mode-style arguments)))
    (when buffer-name
      (rename-buffer (format "*Org src %s*" buffer-name)))
    (when eval-buffer
      (sql-eval-set-buffer eval-buffer))
    (when sql-product
      (setq sql-product sql-product-val))
    (when style
      (setq sql-eval-mode-style (intern-soft style)))))

(defun org-babel-execute:sql (body params)
  "Execute SQL. This function is called by `org-babel-execute-src-block`."
  (with-temp-buffer
    (insert body)
    (shell-execute )
    (sql-eval-buffer-subset (alist-get :eval-buffer params) (point-min) (point-max))))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mirror-image support
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lets you edit images in text in one buffer and see the changes live
;; in another. Source:
;; https://emacs.stackexchange.com/questions/7198/indirect-buffer-in-image-mode-main-buffer-in-text

(defun gpc/mirror-buffer (buffer-name &optional more-after-change)
  "Create a buffer whose contents will follow the current one's
and returns the new buffer.  Runs `more-after-change' after each
change if provided.

This differs from `clone-indirect-buffer' in that the new buffer
is not visiting a file.  It's really just a kludge to support
`gpc/mirror-image', which see."
  (interactive (list
                (let ((default (concat (buffer-name) "<mirror>")))
                  (read-string "Buffer name: " default
                               nil nil default))))
  (make-local-variable 'after-change-functions)
  (make-local-variable 'kill-buffer-hook)
  (lexical-let*
      ((target-buffer (generate-new-buffer buffer-name))
       ;; Give lexical scope to arg
       (after-change more-after-change)
       (copy-change
        #'(lambda(start end old-len)
            (let ((inhibit-read-only t))
              ;; Quick and dirty: may not be suitable for large buffers.
              (copy-to-buffer target-buffer (point-min) (point-max))
              (when (functionp after-change)
                (funcall after-change target-buffer))))))

    ;; Initialize the target buffer with the source text.
    (copy-to-buffer target-buffer (point-min) (point-max))

    (add-hook 'after-change-functions copy-change t t)

    ;; Cleanup hooks.

    ;; Kill the other buffer if the source buffer is closed.
    (add-hook 'kill-buffer-hook
              #'(lambda () (kill-buffer target-buffer)) t t)

    ;; Destroy the change hook if the other buffer is killed.
    (with-current-buffer target-buffer
      (make-local-variable 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook
                #'(lambda ()
                    (remove-hook 'after-change-functions copy-change t))
                t t))))

(defun gpc/mirror-image ()
  "Open an `image-mode' buffer that tracks the content of the
current buffer.  Intended for use with svg files."
  (interactive)
  (image-mode-as-text)
  (let* ((buffer-name (concat (buffer-name) "<image>"))
         ;; An `image-mode' buffer will switch back to text when its contents
         ;; are replaced.  Besides, the image is not updated in-place when the
         ;; content changes, so you'd have to toggle back to image-mode anyway.
         (after-change '(lambda (buffer)
                          (with-current-buffer buffer (image-mode))))
         (mirror (gpc/mirror-buffer buffer-name after-change)))
    (split-window)
    (other-window 1)
    (switch-to-buffer buffer-name)
    (image-mode)
    (other-window 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; image+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package image+
  :ensure t
  :bind (:map imagex-sticky-mode-map
              ("+" . imagex-sticky-zoom-in)
              ("-" . imagex-sticky-zoom-out)
              ("g" . imagex-sticky-restore-original)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Edit server - edit in emacs from Chrome
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'edit-server)
;; (edit-server-start)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; Atomic Chrome - edit with Emacs from Chrome
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package atomic-chrome
;;   :ensure t
;;   :config
;;   (atomic-chrome-start-server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq ns-command-modifier 'meta)      ; Command key is meta
  ;; (setq ns-command-modifier 'super)     ; Command key is super
  ;; (setq mac-right-option-modifier 'meta)
  (setq mac-right-option-modifier 'hyper)
  (setq mac-option-modifier 'meta)
  ;; (setq mac-option-modifier 'super)
  ;; Sadly, doesn't seem to work
  ;; (setq ns-function-modifier 'hyper)
  (setq dock-mode 'docked)
  (message "Switched to docked laptop mode"))

(defun undocked-laptop-mode ()
  "When the laptop isn't connected to my DasKeyboard, different
  bindings work better."
  (interactive)
  (setq ns-command-modifier 'meta)      ; Command key is meta
  (setq mac-right-option-modifier 'control)
  (setq mac-option-modifier 'meta)
  (setq dock-mode 'undocked)
  (message "Switched to undocked laptop mode"))

(defvar dock-mode-timer
  (when (or (string= system-name "valinor")
            (string= system-name "valinor.local"))
    (run-with-timer
     0
     5
     (lambda ()
       (lexical-let ((currently-undocked (= 1920 (display-pixel-width))))
         (when (and (not (eq dock-mode 'undocked))
                    currently-undocked)
           (undocked-laptop-mode))
         (when (and (not (eq dock-mode 'docked))
                    (not currently-undocked))
           (docked-laptop-mode)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compensate for the fact that I frequently fat-finger C-m instead of
;; C-n by first mapping C-m away from return (which will only work
;; when in a GUI session), and then binding C-m
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Caused more problems than it solved.
;; (define-key input-decode-map [?\C-m] [C-m])
;; (global-set-key (kbd "<C-m>") 'next-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; highlight-focus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-bundle kriyative/highlight-focus)

(use-package highlight-focus
  :load-path "highlight-focus"
  :config
  ;; set the background of the mode-line
  (setq highlight-focus:face 'default
        highlight-focus:face-property :background
        highlight-focus:face-property-value "black"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; floobits
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package floobits
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; blockdiag-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package blockdiag-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; graphviz-dot-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package graphviz-dot-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-mind-map
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-mind-map
  :ensure t
  :init (require 'ox-org))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; plantuml-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package plantuml-mode
  :ensure t
  :config
  (progn
    (setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.0/libexec/plantuml.jar")
    (setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.0/libexec/plantuml.jar")
    (add-to-list
     'org-src-lang-modes '("plantuml" . plantuml))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((plantuml . t)))))

;; (use-package flycheck-plantuml
;;   :ensure t
;;   :config
;;   (flycheck-plantuml-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; neotree
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun neotree-resize-window (&rest _args)
    "Resize neotree window.
https://github.com/jaypei/emacs-neotree/pull/110"
    (interactive)
    (neo-buffer--with-resizable-window
     (let ((fit-window-to-buffer-horizontally t))
       (fit-window-to-buffer))))

(use-package neotree
  :ensure t
  :config
  (add-hook 'neo-change-root-hook #'neotree-resize-window)
  (add-hook 'neo-enter-hook #'neotree-resize-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; php-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package php-mode
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; web-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :ensure t)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;; interleave-mode
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Allows you to take notes on a PDF while reading it in Emacs.

;; (use-package interleave
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Capture snippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Credit http://ul.io/nb/2018/04/30/better-code-snippets-with-org-capture/

(which-function-mode 1)

(defun nb/org-capture-get-src-block-string (major-mode)
  "Given a major mode symbol, return the associated org-src block
string that will enable syntax highlighting for that language

E.g. tuareg-mode will return 'ocaml', python-mode 'python', etc..."

  (let ((mm (intern (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))))
    (or (car (rassoc mm org-src-lang-modes)) (format "%s" mm))))

(defun nb/org-capture-region ()
  (interactive)
  (let ((code-snippet (buffer-substring-no-properties (mark) (- (point) 1)))
        (func-name (which-function))
        (file-name (buffer-file-name))
        (line-number (line-number-at-pos (region-beginning)))
        (org-src-mode (nb/org-capture-get-src-block-string major-mode)))
    (kill-new (format
               "file:%s::%s
In ~%s~:
#+BEGIN_SRC %s
%s
#+END_SRC"
               file-name
               line-number
               func-name
               org-src-mode
               code-snippet))))

;; ;; Capture Template
;; ("s" "code snippet" entry (file ,(my/org-dir-file "snippets.org"))
;;  "* %?\n%(my/org-capture-code-snippet \"%F\")")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load a theme
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'paganini t)
(set-default-font-size 180)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; clubhouse integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :ensure t)

(use-package dash-functional
  :ensure t)

(use-package clubhouse-api
  :config
  (setq clubhouse-api-team-name "adzerk")
  (setq clubhouse-api-default-project "Management")
  (setq clubhouse-api-auth-token-path "~/.adzerk-clubhouse-token-default.asc")
  (add-hook 'clubhouse-api-story-edit-minor-mode-hook
            (lambda ()
              (visual-line-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple line references
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-line-reference ()
  "Saves a reference to the current line (like `foo.clj(23)` to the kill ring.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Varibles set by "customize" wind up here
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (functionp 'custom-set-variables-local)
    (custom-set-variables-local))
