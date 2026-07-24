;;; tts.el --- Read the current buffer/region aloud, markdown-aware -*- lexical-binding: t; -*-

;; Author: Craig Andera <candera@wangdera.com>

;; Commentary:
;;
;; Text-to-speech for Emacs on macOS, built on the system `say' command.
;;
;; Markdown is stripped via `pandoc' before speaking, so `#', `*', backticks,
;; link syntax, etc. are not read out. Paragraph/heading boundaries are turned
;; into short spoken pauses (using say's embedded `[[slnc N]]' command) so the
;; reading is paced rather than a flat wall of words.
;;
;; Two ways to listen:
;;
;;   `candera/read-aloud'      -- stream straight to `say'. Simple. Speed is
;;                                fixed for the duration of a reading (set via
;;                                `candera/tts-rate', or adjust and re-read).
;;
;;   `candera/read-aloud-mpv'  -- render to an audio file with `say', then play
;;                                it with mpv. Speed is adjustable *live* while
;;                                playing, without pitch distortion, via mpv's
;;                                JSON IPC socket. Requires `mpv' (brew install
;;                                mpv).
;;
;; The same stop / pause / faster / slower commands work for whichever backend
;; is currently playing.
;;
;; For much better prosody, download a Premium/Siri voice (System Settings ->
;; Accessibility -> Spoken Content -> System Voice -> Manage Voices) and set
;; `candera/tts-voice' to its name.

(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar candera/tts-voice "Serena"
  "Voice passed to `say'. Prefer a downloaded Premium/Siri voice.")

(defvar candera/tts-rate 200
  "Speech rate in words per minute for the plain `say' backend, and the
rate at which audio is rendered for the mpv backend.")

(defvar candera/tts-mpv-speed 1.0
  "Initial playback speed multiplier for the mpv backend.")

(defvar candera/tts-mpv-speed-step 1.15
  "Multiplicative step for `candera/tts-faster'/`candera/tts-slower' in mpv mode.")

(defvar candera/tts-paragraph-pause 500
  "Milliseconds of silence inserted at paragraph/heading boundaries.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar candera/tts--process nil
  "The current `say' or mpv process, if any.")

(defvar candera/tts--mode nil
  "Which backend is currently playing: nil, `say', or `mpv'.")

(defvar candera/tts--paused nil
  "Non-nil when playback is currently paused (say backend only).")

(defvar candera/tts--mpv-socket
  (expand-file-name "candera-tts-mpv.sock" temporary-file-directory)
  "Path to the mpv JSON IPC unix socket.")

(defvar candera/tts--audio-file
  (expand-file-name "candera-tts.aiff" temporary-file-directory)
  "Path to the rendered audio file used by the mpv backend.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text preparation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun candera/tts--plain (start end)
  "Return the text in START..END as speech-ready plain text.
Markdown is flattened with pandoc (falling back to the raw text if
pandoc is unavailable), then paragraph breaks are replaced with an
embedded `say' pause command."
  (let ((raw (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert raw)
      (if (and (executable-find "pandoc")
               (zerop (call-process-region (point-min) (point-max) "pandoc"
                                           t t nil
                                           "-f" "gfm" "-t" "plain"
                                           "--wrap=none")))
          (progn
            ;; Blank-line boundaries (between paragraphs, and around
            ;; headings) become a spoken pause.
            (goto-char (point-min))
            (while (re-search-forward "\n[ \t]*\n+" nil t)
              (replace-match (format " [[slnc %d]] " candera/tts-paragraph-pause)
                             nil t))
            ;; Remaining single newlines just become spaces.
            (goto-char (point-min))
            (while (search-forward "\n" nil t)
              (replace-match " " nil t))
            (string-trim (buffer-string)))
        (string-trim raw)))))

(defun candera/tts--region ()
  "Return a (START . END) pair: the region if active, else the whole buffer."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; say backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun candera/read-aloud (start end)
  "Read the region (or whole buffer) aloud with `say', stripping markdown."
  (interactive (let ((r (candera/tts--region))) (list (car r) (cdr r))))
  (unless (executable-find "say")
    (user-error "The `say' command was not found (macOS only)"))
  (candera/tts-stop)
  (let ((text (candera/tts--plain start end)))
    (setq candera/tts--mode 'say
          candera/tts--paused nil
          candera/tts--process
          (make-process
           :name "candera-tts-say"
           :connection-type 'pipe
           :command (list "say"
                          "-v" candera/tts-voice
                          "-r" (number-to-string candera/tts-rate))
           :sentinel (lambda (proc _event)
                       (unless (process-live-p proc)
                         (setq candera/tts--mode nil)))))
    (process-send-string candera/tts--process text)
    (process-send-eof candera/tts--process)
    (message "Reading aloud with %s at %d wpm" candera/tts-voice candera/tts-rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mpv backend (render then play, with live speed control)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun candera/tts--mpv-send (command)
  "Send COMMAND (a list) to the running mpv over its JSON IPC socket."
  (when (file-exists-p candera/tts--mpv-socket)
    (ignore-errors
      (let ((proc (make-network-process
                   :name "candera-tts-mpv-ipc"
                   :family 'local
                   :service candera/tts--mpv-socket
                   :coding 'utf-8
                   :noquery t)))
        (process-send-string
         proc (concat (json-encode (list (cons "command" (apply #'vector command)))) "\n"))
        (delete-process proc)))))

;;;###autoload
(defun candera/read-aloud-mpv (start end)
  "Render the region (or whole buffer) with `say', then play it in mpv.
Playback speed can be changed live with `candera/tts-faster' and
`candera/tts-slower'."
  (interactive (let ((r (candera/tts--region))) (list (car r) (cdr r))))
  (unless (executable-find "say")
    (user-error "The `say' command was not found (macOS only)"))
  (unless (executable-find "mpv")
    (user-error "mpv not found -- install it with `brew install mpv'"))
  (candera/tts-stop)
  (let ((text (candera/tts--plain start end)))
    (message "Rendering audio...")
    (with-temp-buffer
      (insert text)
      (unless (zerop (call-process-region
                      (point-min) (point-max) "say" nil nil nil
                      "-v" candera/tts-voice
                      "-r" (number-to-string candera/tts-rate)
                      "-o" candera/tts--audio-file))
        (user-error "say failed to render audio")))
    (when (file-exists-p candera/tts--mpv-socket)
      (delete-file candera/tts--mpv-socket))
    (setq candera/tts--mode 'mpv
          candera/tts--paused nil
          candera/tts--process
          (make-process
           :name "candera-tts-mpv"
           :command (list "mpv" "--no-video" "--really-quiet"
                          (format "--speed=%s" candera/tts-mpv-speed)
                          (format "--input-ipc-server=%s" candera/tts--mpv-socket)
                          candera/tts--audio-file)
           :sentinel (lambda (proc _event)
                       (unless (process-live-p proc)
                         (setq candera/tts--mode nil)
                         (when (file-exists-p candera/tts--mpv-socket)
                           (ignore-errors (delete-file candera/tts--mpv-socket)))))))
    (message "Playing in mpv at %.2fx (C-c r f/b to change speed)"
             candera/tts-mpv-speed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transport controls (work for whichever backend is active)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun candera/tts-stop ()
  "Stop any current text-to-speech playback."
  (interactive)
  (when (eq candera/tts--mode 'mpv)
    (candera/tts--mpv-send '("quit")))
  (when (process-live-p candera/tts--process)
    (delete-process candera/tts--process))
  (setq candera/tts--process nil
        candera/tts--mode nil
        candera/tts--paused nil))

;;;###autoload
(defun candera/tts-toggle-pause ()
  "Pause or resume the current playback."
  (interactive)
  (pcase candera/tts--mode
    ('mpv (candera/tts--mpv-send '("cycle" "pause")))
    ('say (when (process-live-p candera/tts--process)
            (signal-process candera/tts--process
                             (if candera/tts--paused 'SIGCONT 'SIGSTOP))
            (setq candera/tts--paused (not candera/tts--paused))))
    (_ (message "Nothing is playing"))))

;;;###autoload
(defun candera/tts-faster ()
  "Speed up playback.
In mpv mode this takes effect immediately; in say mode it changes the
rate used for the next reading."
  (interactive)
  (if (eq candera/tts--mode 'mpv)
      (progn
        (setq candera/tts-mpv-speed (* candera/tts-mpv-speed candera/tts-mpv-speed-step))
        (candera/tts--mpv-send (list "set" "speed" candera/tts-mpv-speed))
        (message "Speed: %.2fx" candera/tts-mpv-speed))
    (setq candera/tts-rate (+ candera/tts-rate 25))
    (message "Rate: %d wpm (re-read to apply)" candera/tts-rate)))

;;;###autoload
(defun candera/tts-slower ()
  "Slow down playback (see `candera/tts-faster')."
  (interactive)
  (if (eq candera/tts--mode 'mpv)
      (progn
        (setq candera/tts-mpv-speed (max 0.25 (/ candera/tts-mpv-speed candera/tts-mpv-speed-step)))
        (candera/tts--mpv-send (list "set" "speed" candera/tts-mpv-speed))
        (message "Speed: %.2fx" candera/tts-mpv-speed))
    (setq candera/tts-rate (max 50 (- candera/tts-rate 25)))
    (message "Rate: %d wpm (re-read to apply)" candera/tts-rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings: C-c r prefix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-prefix-command 'candera/tts-map)
(global-set-key (kbd "C-c r") 'candera/tts-map)
(define-key candera/tts-map (kbd "r")   #'candera/read-aloud)
(define-key candera/tts-map (kbd "m")   #'candera/read-aloud-mpv)
(define-key candera/tts-map (kbd "s")   #'candera/tts-stop)
(define-key candera/tts-map (kbd "SPC") #'candera/tts-toggle-pause)
(define-key candera/tts-map (kbd "f")   #'candera/tts-faster)
(define-key candera/tts-map (kbd "b")   #'candera/tts-slower)

(provide 'tts)

;;; tts.el ends here
