;;; slack-notes.el --- Transform refs to people to slack handles in exported text  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'dash)
(require 'ox-slack)

(defgroup slack-notes nil
  "Conversions between first names and Slack handles."
  :group 'productivity
  :prefix "slack-notes-")

(defcustom slack-notes-name-translation-alist nil
  "Alist of regular expressions to Slack handles."
  :group 'slack-notes
  :type '(alist string string))



(defun slack-notes-translate-names (str)
  (seq-reduce
   (-lambda (acc (name . handle))
     (replace-regexp-in-string (rx-to-string `(and bow (regexp ,name) eow)
                                             t)
                               handle
                               acc t))
   slack-notes-name-translation-alist
   str))

(defun slack-notes-copy ()
  "Export the subtree at point as Slack markdown."
  (interactive)
  (cl-assert (org-at-heading-p))
  (let ((heading (org-get-heading)))
    (ox-slack-export-to-clipboard nil
                                  t
                                  nil
                                  nil
                                  nil
                                  (lambda (str)
                                    (format "*%s*\n\n%s" (string-trim heading) str)))))

(cl-eval-when (load compile eval)
  (add-to-list 'safe-local-variable-values '(ox-slack-postprocess-function . slack-notes-translate-names)))

(provide 'slack-notes)

;;; slack-notes.el ends here
