;;; ol-linux-header.el --- Link to Linux headers  -*- lexical-binding: t; -*-

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

(require 'ol)
(require 'memoize)

(autoload 'magit-find-file "magit")
(autoload 'magit-git-lines "magit")

(defvar linux-src-dir "~/src/linux")

(defun org-link-linux-src-files-for-rev (rev)
  (magit-git-lines "ls-tree" "-r" "--name-only" rev))

(ignore-errors
  (memoize 'org-link-linux-src-files-for-rev))

(defun org-link-follow-linux-src (path &rest _)
  (pcase-let ((`(,rev ,file) (split-string path ":")))
    (if (and linux-src-dir (file-directory-p linux-src-dir))
        (let ((default-directory linux-src-dir))
          (magit-find-file rev file))
      (browse-url (format "https://github.com/torvalds/linux/blob/%s/%s" rev file)))))

(defun org-link-complete-linux-src ()
  (let* ((default-directory linux-src-dir)
         (rev (completing-read "Rev: " (append '("HEAD") (magit-git-lines "tag"))))
         (norm (substring (car (magit-git-lines "rev-parse" rev)) 0 6)))
    (concat "linux:" norm ":" (completing-read "File: " (org-link-linux-src-files-for-rev rev)))))

(org-link-set-parameters "linux"
                         :complete #'org-link-complete-linux-src
                         :follow #'org-link-follow-linux-src)

(provide 'ol-linux-header)

;;; ol-linux-header.el ends here
