;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactively evaluate SPARQL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; URL encoding for parameters
(defun http-url-encode (str)
  "URL encode STR."
  (apply 'concat
          (mapcar (lambda (c)
                       (if (or (and (>= c ?a) (<= c ?z))
                                  (and (>= c ?A) (<= c ?Z))
                                     (and (>= c ?0) (<= c ?9)))
                                  (string c)
                              (format "%%%02x" c)))
                   (encode-coding-string str 'utf-8))))

(defvar sparql-base-url nil)
(defconst sparql-default-base-url  "http://localhost:2020/metamodl_test")

(defun sparql-set-base-url (url)
  "Sets the base URL for queries"
  ;; TODO: This isn't displaying the prompt for some reason
  (interactive "sNew base URL for queries: ")
  (setq sparql-base-url url))

(make-variable-buffer-local 'sparql-base-url)

(defun sparql-get-base-url ()
  "Returns the base URL for SPARQL queries in this buffer unless it has not been set, in which case it prompts the user."
  (if sparql-base-url
      sparql-base-url
    (setq sparql-base-url 
          (read-string
           (format "SPARQL URL (%s): " sparql-default-base-url) 
           nil
           nil
           sparql-default-base-url))))

(defun sparql-query-region ()
  "Submit the active region as a query to a SPARQL HTTP endpoint.
If the region is not active, use the whole buffer."
  (interactive)
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring beg end))
         (escaped-text (http-url-encode text))
         ;; TODO: Stop hardcoding this at some point
         (url (format "%s?format=csv&query=%s"
                      (sparql-get-base-url) escaped-text))
         (b (url-retrieve url 
                          #'(lambda (status &rest cbargs)))))
    (switch-to-buffer-other-window b)))

(defconst sparql-keywords 
  '(("#.*$" . font-lock-comment-face)
    ("SELECT\\|ASK\\|WHERE\\|IN\\|COALESCE\\|PREFIX\\|ORDER\\|GROUP\\|BY\\|LET\\|OPTIONAL" . font-lock-keyword-face)
    ("\\?\\w+" . font-lock-variable-name-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face)))

(define-derived-mode sparql-mode text-mode
  "SPARQL"
  :group 'sparql-mode
  (setq font-lock-defaults '(sparql-keywords))
  (define-key sparql-mode-map (kbd "C-c x") 'sparql-query-region))

(provide 'sparql-mode)