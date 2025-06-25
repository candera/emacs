;;; view-visited-file.el --- Displays the visited file in the minibuffer

;; Copyright (C) 2008 Wangdera Corporation

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; Author: Craig Andera <candera@wangdera.com>

;; Commentary: Invoke this minor mode to have the visited file
;; displayed in the mode-line. To use, just load this file and invoke
;; (view-visited-file-mode) or (turn-on-view-visited-file-mode).

(defun view-visited-file ()
  "Displays the visited file in the minibuffer"
  (interactive)
  (kill-new buffer-file-name)
  (message buffer-file-name))

(global-set-key (kbd "C-c v") 'view-visited-file)
(global-set-key (kbd "C-c C-v") 'view-visited-file)

(provide 'view-visited-file)
