;;; org-roam-search.el --- A search interface that works better with org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

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

;; Org-roam works best when your notes are divided into many files, but this
;; makes the org-search functionality unsuitable. ripgrep does a better job, but
;; has the problem that it shows the raw filenames instead of the note title.

;; This implementation of search aims to surface matched text along with the
;; title of the relevant note, when available.

;;; Code:

(require 'consult)
(require 'memoize)
(require 'org-roam)

(defun org-roam-search--replace-links-in-string (str)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))

      ;; Replace links with their descriptions.
      (save-excursion
        (while (search-forward-regexp org-link-bracket-re nil t)
          (replace-match (match-string 2))))

      ;; Best-effort processing for remaining line-wrapped links
      (save-excursion
        (while (search-forward-regexp (rx "[[" (+? nonl) "][" (group (+? nonl)) (? "]")) nil t)
          (replace-match (match-string 1))))

      (buffer-substring (point-min) (point-max)))))

(defun org-roam-search--candidate-group (cand transform)
  "Return title for CAND or TRANSFORM the candidate."
  (let* ((line (substring cand (1+ (length (get-text-property 0 'consult--grep-file cand)))))
         (filename (get-text-property 0 'consult--grep-file cand)))
    (if transform
        (org-roam-search--replace-links-in-string line)
      (org-roam-search--format-group-title filename))))

(defconst org-roam-search-title-search-byte-limit 1024)

(defun org-roam-search--lookup-title (file)
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name file org-roam-directory) nil nil org-roam-search-title-search-byte-limit)
    (goto-char (point-min))
    (if (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
        (match-string 1)
      file)))

(defun org-roam-search--format-group-title (file)
  (let ((title (org-roam-search--lookup-title file))
        (dir (-some->> (file-name-directory file) (string-remove-prefix "/") (string-remove-suffix "/"))))
    (if (or (null dir) (string-blank-p dir))
        title
      (format "%s > %s" dir title))))

(ignore-errors
  (memoize 'org-roam-search--format-group-title 60))

(defun org-roam-search (&optional initial)
  "Search for regexp with rg in `org-roam-directory' with INITIAL input."
  (interactive)
  (let* ((default-directory org-roam-directory)
         (read-process-output-max (max read-process-output-max (* 1024 1024))))
    (consult--read
     (consult--async-command #'consult--ripgrep-builder
       (consult--grep-format #'consult--ripgrep-builder)
       :file-handler t)
     :prompt "Search Roam: "
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :initial (consult--async-split-initial initial)
     :add-history (consult--async-split-thingatpt 'symbol)
     :require-match t
     :category 'consult-grep
     :group #'org-roam-search--candidate-group
     :history '(:input consult--grep-history)
     :sort nil)))

(provide 'org-roam-search)

;;; org-roam-search.el ends here
