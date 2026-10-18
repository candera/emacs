;;; craigandera-links.el --- Insert links to craigandera.org pages -*- lexical-binding: t; -*-

(defvar candera/site-root "~/projects/craigandera.org/"
  "Fallback root of the craigandera.org Hugo project.")

(defun candera/site--root ()
  "Root of the Hugo project containing the current buffer, or the fallback."
  (file-name-as-directory
   (expand-file-name
    (or (locate-dominating-file default-directory "hugo.toml")
        candera/site-root))))

(defun candera/site--header (file)
  "Return an alist of lowercase #+KEY values from the head of FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 4000)
    (goto-char (point-min))
    (let (alist)
      (while (looking-at "^#\\+\\([a-zA-Z]+\\(?:\\[\\]\\)?\\):[ \t]*\\(.*\\)$")
        (push (cons (downcase (match-string 1)) (match-string 2)) alist)
        (forward-line 1))
      alist)))

(defun candera/site--url (root file)
  "Site-absolute URL for content FILE under ROOT, or nil if it has none."
  (let ((rel (file-relative-name file (expand-file-name "content/" root))))
    (cond ((string-prefix-p "gallery/" rel) nil)
          ((string-match "\\`\\(.*/\\)?\\(?:_index\\|index\\)\\.org\\'" rel)
           (concat "/" (match-string 1 rel)))
          ((string-match "\\`\\(.*\\)\\.org\\'" rel)
           (concat "/" (match-string 1 rel) "/")))))

(defun candera/site-link-candidates ()
  "Alist of (DISPLAY . (URL . TITLE)) for every non-draft page on the site."
  (let* ((root (candera/site--root))
         (content (expand-file-name "content/" root))
         result)
    (dolist (file (directory-files-recursively content "\\.org\\'"))
      (let* ((hdr (candera/site--header file))
             (title (cdr (assoc "title" hdr)))
             (url (candera/site--url root file)))
        (when (and title url
                   (not (equal (cdr (assoc "draft" hdr)) "true")))
          (let ((section (if (string-match "\\`/\\([^/]+\\)/." url)
                             (match-string 1 url)
                           "site")))
            (push (cons (format "%s  [%s]" title section)
                        (cons url title))
                  result)))))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun candera/site-insert-link ()
  "Insert an Org link to a craigandera.org page, chosen by title.
With an active region, the region becomes the link text; otherwise
the page's title does."
  (interactive)
  (let* ((cands (candera/site-link-candidates))
         (choice (cdr (assoc (completing-read "Site page: " cands nil t) cands)))
         (url (car choice))
         (text (if (use-region-p)
                   (prog1 (buffer-substring-no-properties
                           (region-beginning) (region-end))
                     (delete-region (region-beginning) (region-end)))
                 (cdr choice))))
    (insert (format "[[%s][%s]]" url text))))

(defun candera/site-insert-link-dwim ()
  "Insert a link to a site page; with a prefix arg, use `org-insert-link'.
The prefix arg is consumed, so C-u gives the plain `org-insert-link'
(handy for external URLs) rather than its file-completion variant."
  (interactive)
  (if current-prefix-arg
      (let ((current-prefix-arg nil))
        (call-interactively #'org-insert-link))
    (call-interactively #'candera/site-insert-link)))

(defvar craigandera-site-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'candera/site-insert-link-dwim)
    map)
  "Keymap for `craigandera-site-mode'.")

;;;###autoload
(define-minor-mode craigandera-site-mode
  "Editing helpers for the craigandera.org Hugo site.
Enabled by the site's .dir-locals.el."
  :lighter " Site"
  :keymap craigandera-site-mode-map)

(provide 'craigandera-links)
;;; craigandera-links.el ends here
