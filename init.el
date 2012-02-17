(defun load-file-if-exists (path)
  (if (file-exists-p path)
      (load-file path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load nxhtml-mode (with MuMaMo)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load "~/.emacs.d/custom/nxhtml/autostart.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start an emacs server except on Windows,
;; where it would open a TCP socket. But don't
;; start one if one is already running
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (or (eq system-type 'windows-nt)
            (server-running-p))
  (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Do any initialization that's specific to this machine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file-if-exists "~/local-init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up the load-path
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/custom/candera/")
(add-to-list 'load-path "~/.emacs.d/custom/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stuff we want to load right away
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

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

;; Enable CUA-style undo, cut, copy, paste, and selection
;;(cua-mode t)

;; For the love of all that is holy, unify the emacs kill ring and the
;; clipboard
(setq x-select-enable-clipboard t)

;; And don't allow tabs to be inserted
(setq-default indent-tabs-mode nil)

;; Enable ido-mode
(ido-mode 1)

;; Enable line numbering globally
(global-linum-mode 1)
;; And add a space after the line number in text-only terminals
(unless window-system (setq linum-format "%d "))

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
  "Stores the window that was in effect when center-window-horizontally was called.")

(defun center-window-horizontally (width)
  "Arrange windows three as side-by-side, with the center one
having width WIDTH.
Accepts WIDTH as a numeric prefix, but defaults to 85."
  (interactive "P")
  (setq former-window-configuration (current-window-configuration))
  (let ((width (if width width 85)))
    (let ((side-window-width (/ (- (frame-parameter nil 'width) width) 2)))
      (delete-other-windows)
      (split-window-horizontally side-window-width)
      (other-window 1)
      (split-window-horizontally (- side-window-width)))))

(defun restore-former-window-configuration ()
  "Restore the window configuration that was in effect before
  `center-window-horizontally' was called."
  (interactive)
  (set-window-configuration former-window-configuration))

(global-set-key (kbd "C-x 4 C-c") 'center-window-horizontally)
(global-set-key (kbd "C-x 4 C-r") 'restore-former-window-configuration)

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
(global-set-key (kbd "C-;") 'flyspell-auto-correct-previous-word)

(global-set-key (quote [C-tab]) 'other-window)

(global-set-key [f12] 'shell)
(global-set-key [f4] 'call-last-kbd-macro)
(global-set-key [f8] 'next-error)
(column-number-mode ())

(setq default-major-mode 'text-mode)
(setq display-time-day-and-date 'true)
(display-time)

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

(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'javascript-mode-hook 'turn-off-flyspell)
(add-hook 'emacs-lisp-mode-hook 'turn-off-flyspell)
(add-hook 'ruby-mode-hook 'turn-off-flyspell)

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

(add-hook 'emacs-lisp-mode-hook 'setup-highlight-whitespace)
(add-hook 'text-mode-hook 'setup-highlight-whitespace)
(add-hook 'lisp-mode-hook 'setup-highlight-whitespace)
(add-hook 'ruby-mode 'setup-highlight-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set M-. to do imenu rather than find-tag
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-.") 'imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set M-' to do completion
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-'") 'completion-at-point)

(add-hook 'slime-mode-hook
          (lambda ()
            (define-key slime-mode-map (kbd "M-'") 'slime-complete-symbol)))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-key slime-repl-mode-map (kbd "M-'") 'slime-complete-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up emacs-lisp-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

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
   '( (font . "-outline-Courier New-bold-r-normal-normal-19-142-96-96-c-110-iso10646-1"))))

(defun set-small-font ()
  "sets the font to something readable from more than 3 inches away"
  (interactive)
  (modify-frame-parameters
   nil
   '( (font . "-outline-Courier New-bold-r-normal-normal-12-142-96-96-c-110-iso10646-1"))))

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

;; Make ido-mode list things vertically
(setq ido-decorations
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))   ; confirm creation of new file or buffer

;; And let us use standard navagation keys that make sense vertically
(add-hook 'ido-setup-hook
 '(lambda ()
    (define-key ido-completion-map [down] 'ido-next-match)
    (define-key ido-completion-map [up] 'ido-prev-match)
    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;; Tell emacs to wrap lines in vertically split windows
(setq truncate-partial-width-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up dired-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dired-insert-this-directory-recursively ()
  "Recursively insert the subdirectories of the current dired directory."
  (interactive)
  (dired-insert-subdir dired-directory "-alR"))

(add-hook 'dired-mode-hook
          '(lambda ()
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

(add-hook 'ruby-mode-hook (lambda () (define-key ruby-mode-map (kbd "M-.") 'imenu)))
(define-key emacs-lisp-mode-map (kbd "M-.") 'imenu)
(add-hook 'javascript-mode-hook (lambda () (define-key js-mode-map (kbd "M-.") 'imenu)))

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

(add-to-list 'load-path "~/.emacs.d/custom/slime-2010-05-19/")  ; your SLIME directory
(add-to-list 'load-path "~/.emacs.d/custom/slime-2010-05-19/contrib")  ; your SLIME contrib directory


;;(add-to-list 'load-path "~/emacs/slime-2008-11-03/")  ; your SLIME directory
;;(add-to-list 'load-path "~/emacs/slime-2008-11-03/contrib")  ; your SLIME contrib directory
;; (setq
;;   ; inferior-lisp-program "C:/bin/clisp-2.45/clisp -K full"  ; your Lisp system
;;   ;inferior-lisp-program "C:/bin/sbcl-1.0.14.22/sbcl --core C:/bin/sbcl-1.0.14.22/sbcl.core"  ; your Lisp system
;;   slime-complete-symbol-function 'slime-fuzzy-complete-symbol  ; fuzzy symbol completion (requires slime-fuzzy from contrib)
;;   ;slime-complete-symbol-function 'slime-complete-symbol  ; standard symbol completion
;;   lisp-indent-function 'common-lisp-indent-function            ; How would you like to indent?
;;  )

(require 'slime-autoloads)
(slime-setup '(slime-repl slime-editing-commands slime-fuzzy slime-presentations slime-scratch))
;(slime-setup '(slime-fancy))

;(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)  ; fuzzy symbol completion (requires slime-fuzzy from contrib)

(if (functionp 'slime-local-setup)
    (slime-local-setup))

;; Turn off the annoying SLIME version mismatch message

(eval-after-load 'slime '(setq slime-protocol-version 'ignore))

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

;; This is a total hack: we're hardcoding the name of the shell buffer
(defun shell-send-input (input)
  "Send INPUT into the *shell* buffer and leave it visible."
  (save-selected-window
    (switch-to-buffer-other-window "*shell*")
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
  (shell-send-input (buffer-substring-no-properties
                     (region-beginning)
                     (region-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Require clojure-mode to load and associate it to all .clj files.
(add-to-list 'load-path "~/.emacs.d/custom/clojure-mode/")

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map (kbd "C-c e") 'shell-eval-last-expression)
             (define-key clojure-mode-map (kbd "C-c x") 'shell-eval-defun)))

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

;; These are extra key defines because I kept typing them.
;; Within clojure-mode, have Ctrl-x Ctrl-e evaluate the last
;; expression.
;; Ctrl-c Ctrl-e is also there, because I kept typoing it.
(add-hook 'clojure-mode-hook
          '(lambda ()
;            (highlight-parentheses-mode 1)
             (linum-mode 1)
             (paredit-mode 1)
             (flyspell-mode 0)
             (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)
             (define-key clojure-mode-map "\C-x\C-e" 'lisp-eval-last-sexp)
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
             (define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)))

(require 'clojure-test-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elein-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/custom/elein")
(require 'elein)

(defun lein-repl ()
  "Run 'lein repl' in an inferior-lisp."
  (interactive)
  (inferior-lisp "lein repl"))

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
(add-hook 'text-mode-hook (lambda ()
                            (turn-on-typing-speed)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Enable xml-lite for editing XML
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path (expand-file-name "~/emacs/xml-lite"))
;; (autoload 'xml-lite-mode "xml-lite" "Major mode to edit XML files" t)
;; (setq auto-mode-alist
;;       (append '(
;;              ("\\.xml$" . sgml-mode)
;;              ("\\.build$" . sgml-mode)
;;              ("\\.config$" . sgml-mode)
;;              ("\\.xslt$" . sgml-mode)
;;              ("\\.xsl$" . sgml-mode)
;;              ("\\.xsd$" . sgml-mode)
;;              ) auto-mode-alist ))
;; (add-hook 'sgml-mode-hook 'xml-lite-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Set up the command shell
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-shell-setup ()
  "For cmdproxy shell under Emacs 20"
  (setq w32-quote-process-args ?\")
  (make-variable-buffer-local 'comint-completion-addsuffix))

(setq shell-mode-hook 'my-shell-setup)

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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom/flexwiki"))
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

;(add-to-list 'load-path "/path/to/highlight-symbol")
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

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up magit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/custom/magit")
(require 'magit)

;; C-x m is normally compose-mail, but I never do mail inside of
;; emacs, whereas I run magit-status all the time
(global-set-key (kbd "C-x m") 'magit-status)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up gist.el
;;
;; http://github.com/defunkt/gist.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/custom/gist.el")
(require 'gist)

;; Turn on longlines mode whenever we're in an edit server buffer
(add-hook 'edit-server-text-mode-hook
          '(lambda ()
             (longlines-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up later version of org-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/.emacs.d/custom/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/custom/org-mode/contrib/lisp" load-path))
(require 'org-install)

(setq org-todo-keywords (quote ((sequence "TODO" "INPROGRESS" "DONE"))))

;; org-mode refuses to invoke org-indent-mode in emacs 23, claiming
;; that it might crash. So I set this variable, which gets me the same
;; effect.
(setq org-hide-leading-stars t)

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
;; Set up coffee-mode
;;
;; http://ozmm.org/posts/coffee_mode.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/custom/coffee-mode")
(require 'coffee-mode)

;; Uncomment if necessary
;; (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;; (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

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

(add-to-list 'load-path "~/.emacs.d/custom/sparql-mode")
(autoload 'sparql-mode "sparql-mode.el"
     "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))

(setq sparql-default-base-url "http://localhost:2020/metamodl_test/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Miscellaneous customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq global-hl-line-mode nil)

(setq confirm-nonexistent-file-or-buffer nil)

(load "~/.emacs.d/custom/colors.el")

(add-to-list 'auto-mode-alist '("\\.az$" . java-mode))
(add-to-list 'auto-mode-alist '("\\.asc$" . javascript-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mac customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
 (setq ns-command-modifier 'meta)       ; Command key is meta
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Varibles set by "customize" wind up here
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (functionp 'custom-set-variables-local)
    (custom-set-variables-local))
