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
;; where it would open a TCP socket
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (eq system-type 'windows-nt)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; io-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/custom/io-mode")
;; (require 'io-mode)

;; (defun io-repl-line ()
;;   (interactive)
;;   (let ((source-buffer (current-buffer)))
;;     (message (buffer-name source-buffer))
;;     (save-excursion
;;       (beginning-of-line)
;;       (let ((start (point)))
;;      (end-of-line)
;;      (io-repl-sregion start (point))))
;;     (pop-to-buffer source-buffer)))

;; (define-key io-mode-map (kbd "C-M-X") 'io-repl-line)

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
(setq-default insert-tabs-mode nil)

;; Enable iswitchb-mode
;;(iswitchb-mode 1)

;; Or enable ido-mode
(ido-mode 1)

;; Enable line numbering globally
(global-linum-mode 1)
;; And add a space after the line number in text-only terminals
(unless window-system (setq linum-format "%d "))

;; Enable line highlighting
(global-hl-line-mode 1)

;; Enable rainbow parens
;(highlight-parentheses-mode 1)

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
(menu-bar-mode -1)

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

;; And define shift-bindings for modes (like shell) that override the control ones
;; (global-set-key (quote [M-S-down]) 'scroll-other-window-down-one)
;; (global-set-key (quote [M-S-up])   'scroll-other-window-up-one)
;; (global-set-key (quote [S-up]) 'scroll-window-up-one)
;; (global-set-key (quote [S-down]) 'scroll-window-down-one)

;; (global-set-key (quote [C-tab]) 'iswitchb-buffer)

;; (global-unset-key [home])
;; (global-unset-key [end])

;; (global-set-key [home] 'beginning-of-line)
;; (global-set-key [end] 'end-of-line)
;; (global-set-key (quote [C-home]) (quote beginning-of-buffer))
;; (global-set-key (quote [C-end]) (quote end-of-buffer))
(global-set-key "\C-z" 'undo)

;; (defun switch-to-default-buffer ()
;;   "Switches to whatever buffer is the default switch-to"
;;   (interactive)
;;   (switch-to-buffer nil))

(global-set-key (quote [C-tab]) 'other-window)

(global-set-key [f12] 'shell)
(global-set-key [f4] 'call-last-kbd-macro)
(global-set-key [f8] 'next-error)
(column-number-mode ())

(setq default-major-mode 'text-mode)
(setq display-time-day-and-date 'true)
(display-time)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
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

;; Friends don't let friends save source code with tabs in it
(defun detabify-buffer ()
  "Calls untabify on the current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defvar detabify-modes '(javascript-mode emacs-lisp-mode ruby-mode clojure-mode)
  "A list of the modes that will have tabs converted to spaces before saving.")

(defun mode-aware-detabify ()
  "Calls untabify on the current buffer if the major mode is one of 'detabify-modes'"
  (interactive)
  (when (member major-mode detabify-modes)
    (detabify-buffer)))

(add-hook 'before-save-hook 'mode-aware-detabify)

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
;; (setq ido-decorations 
;;       (quote 
;;        ("\n-> "           ; Opening bracket around prospect list
;;      ""                ; Closing bracket around prospect list
;;      "\n   "           ; separator between prospects
;;      "\n   ..."        ; appears at end of truncated list of prospects
;;      "["               ; opening bracket around common match string
;;      "]"               ; closing bracket around common match string
;;      " [No match]"     ; displayed when there is no match
;;      " [Matched]"      ; displayed if there is a single match
;;      " [Not readable]" ; current diretory is not readable
;;      " [Too big]"      ; directory too big
;;      " [Confirm]")))   ; confirm creation of new file or buffer

;; Tell emacs to wrap lines in vertically split windows
(setq truncate-partial-width-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Associate javascript mode with .js files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up simple refactoring support
;; (only symbol renaming at this point)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'refactor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Autocomplete
;; http://cx4a.org/software/auto-complete/
;; https://github.com/m2ym/auto-complete.git
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-to-list 'load-path "~/.emacs.d/custom/auto-complete")
;(require 'auto-complete)

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

(require 'slime)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This section sets up clojure-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; Require clojure-mode to load and associate it to all .clj files.
(add-to-list 'load-path "~/.emacs.d/custom/clojure-mode/")

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))


;; Turn on paredit for clojure files
;(require 'clojure-paredit)
(setq clojure-enable-paredit t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set up java doc browser for clojure
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slime-java-describe (symbol-name) 
  "Get details on Java class/instance at point." 
  (interactive (list (slime-read-symbol-name "Java Class/instance: "))) 
  (when (not symbol-name) 
    (error "No symbol given")) 
  (save-excursion 
    (set-buffer (slime-output-buffer)) 
    (unless (eq (current-buffer) (window-buffer)) 
      (pop-to-buffer (current-buffer) t)) 
    (goto-char (point-max)) 
    (insert (concat "(show " symbol-name ")")) 
    (when symbol-name 
      (slime-repl-return) 
      (other-window 1))))
 
(defun slime-javadoc (symbol-name) 
  "Get JavaDoc documentation on Java class at point." 
  (interactive (list (slime-read-symbol-name "JavaDoc info for: "))) 
  (when (not symbol-name) 
    (error "No symbol given")) 
  (set-buffer (slime-output-buffer)) 
  (unless (eq (current-buffer) (window-buffer)) 
    (pop-to-buffer (current-buffer) t)) 
  (goto-char (point-max)) 
  (insert (concat "(javadoc " symbol-name ")")) 
  (when symbol-name 
    (slime-repl-return) 
    (other-window 1)))
 
(add-hook 'slime-connected-hook (lambda () 
                                  (interactive) 
;; slime-redirect-inferior-output causes problems with slime-connect
;; workaround available here:
;; http://github.com/technomancy/swank-clojure/issues/issue/18
;(slime-redirect-inferior-output) 
(define-key slime-mode-map (kbd "C-c d") 'slime-java-describe) 
(define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe) 
(define-key slime-mode-map (kbd "C-c D") 'slime-javadoc) 
(define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc))) 


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

;;(defvar *journal-roots* '("Z:/daily/" "C:/data/daily/"))
(load-file "~/.emacs.d/custom/candera/journal.el")
(global-set-key "\C-x\C-y" 'find-yesterday-log-file)


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
; Set bash as the preferred shell
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For the interactive shell
; (setq explicit-shell-file-name "bash.exe")

;; For subprocesses invoked via the shell (e.g., "shell -c command")
; (setq shell-file-name explicit-shell-file-name)

;; (defun my-shell-setup ()
;;   "For Cygwin bash under Emacs 20"
;;   (setq comint-scroll-show-maximum-output 'this)
;;   (setq comint-completion-addsuffix t)
;;   ;; (setq comint-process-echoes t) ;; reported that this is no longer needed
;;   (setq comint-eol-on-send t)
;;   (setq w32-quote-process-args ?\")
;;   (make-variable-buffer-local 'comint-completion-addsuffix))

;; (setq shell-mode-hook 'my-shell-setup)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up gist.el
;;
;; http://github.com/defunkt/gist.el 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/custom/gist.el")
(require 'gist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start an edit server for the Chrome extension that lets you edit
;; rich text areas using emacs: 
;; http://github.com/stsquad/emacs_chrome
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'edit-server)
(edit-server-start)

;; Turn on longlines mode whenever we're in an edit server buffer
(add-hook 'edit-server-text-mode-hook 
          '(lambda ()
             (longlines-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support mo-git-blame
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/custom/mo-git-blame")
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set up later version of org-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/.emacs.d/custom/org-7.4/lisp" load-path))
(setq load-path (cons "~/.emacs.d/custom/org-7.4/contrib/lisp" load-path))
(require 'org-install)

(setq org-directory "~/Dropbox/")
(setq org-mobile-directory "~/Dropbox/MobileOrg/")
(setq org-agenda-files '("~/Dropbox/notes.txt"))
(setq org-mobile-inbox-for-pull "~/Dropbox/inbox.org")

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
;; Miscellaneous customizations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq global-hl-line-mode nil)

(setq confirm-nonexistent-file-or-buffer nil)

;; '(default ((t (:stipple
;;             nil :background "grey10" :foreground "white" :inverse-video
;;             nil :box nil :strike-through nil :overline nil :underline
;;             nil :slant normal :weight normal :height 140 :width
;;             normal :foundry "outline" :family "Courier New"))))

 ;; '(default ((t (:stipple nil :background "grey10" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 50 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))

;; Oddly, setting the background to black under a terminal turns it
;; grey. But the default in a windowed system is black on white. Sigh.
(when window-system
  (set-face-background 'default "black")
  (set-face-foreground 'default "white"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 `(default ((t ,(if (eq system-type 'windows-nt)
                    '(:foundry "outline" 
                      :family "Courier New"
                      :weight bold
                      :height 160
                      :width normal)
                  '(:family "DejaVu Sans Mono" :foundry "unknown" :height 140)
))))
 '(mode-line ((((class color) (min-colors 88)) (:background "#8888ff" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(cursor ((t (:background "red"))))
 '(highline-face ((t (:background "grey20"))))
 '(hl-line ((t (:inherit highlight :background "grey20"))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "lightgreen"))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "grey20"))))
 '(nxml-attribute-colon-face ((t (:inherit nxml-name-face :foreground "blue"))))
 '(nxml-attribute-local-name-face ((t (:inherit nxml-name-face :foreground "blue"))))
 '(nxml-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "blue"))))
 '(nxml-namespace-attribute-colon-face ((t (:inherit nxml-name-face :foreground "red"))))
 '(nxml-namespace-attribute-prefix-face ((t (:inherit nxml-name-face :foreground "red"))))
 '(nxml-namespace-attribute-xmlns-face ((t (:inherit nxml-name-face :foreground "red"))))
 )

(unless window-system
 (custom-set-faces 
  '(isearch-face ((t (:foreground "black" :background "yellow"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Varibles set by "customize" wind up here
; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (functionp 'custom-set-variables-local)
    (custom-set-variables-local))
