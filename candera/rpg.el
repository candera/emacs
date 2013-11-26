;;; rpg.el --- useful functions for runing a role playing game
;;
;; Copyright (C) 2013 Craig Andera
;;
;; Author: Craig Andera <rpg.el@craigandera.org>
;; Version: 1.0
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (add-to-list 'load-path "/path/to/containing/dir")
;; (require 'rpg)
;;
;;; Code:

(defun rpg-roll-fate-dice ()
  "Rolls Fate dice and returns the result."
  (+ -2
     (random 2)
     (random 2)
     (random 2)
     (random 2)))

(defun rpg-fate-dice ()
  "Rolls Fate dice and displays the result in the echo area."
  (interactive)
  (message (format "%d" (rpg-roll-fate-dice))))

;; (defun rpg-test-fate-dice ()
;;   ""
;;   (interactive)
;;   (let ((h (make-hash-table)))
;;     (dotimes (i 1000)
;;       (let ((d (rpg-roll-fate-dice)))
;;         (puthash d (+ 1 (gethash d h 0)) h)))
;;     h))

(provide 'rpg)
