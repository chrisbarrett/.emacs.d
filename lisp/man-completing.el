;;; man-completing.el --- Lookup manpages with completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Chris Barrett

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
(require 'f)
(require 'ht)
(require 'memoize)
(require 'interpolate)
(require 'man)

(defgroup man-completing nil
  "Use manpath for manpage completion."
  :group 'productivity
  :prefix "man-completing-")

(defun man-completing--list-manpages ()
  (let ((paths (split-string (string-trim (shell-command-to-string "manpath -q")) ":" t)))
    (ht-from-alist (seq-mapcat (lambda (dir)
                                 (let ((files (f-files dir (lambda (file) (not (equal "gz" (f-ext file)))) t)))
                                   (seq-map (lambda (file)
                                              (cons (f-filename file)
                                                    (list :name (f-base file)
                                                          :section (f-ext file))))
                                            files)))
                               paths))))
(ignore-errors
  (memoize 'man-completing--list-manpages))

(defun man-completing--read ()
  "Run `man' with completion.

Uses `completing-read' to prompt for a manpage if the symbol at
point is ambiguous."
  (interactive)
  (let* ((completion-ignore-case t)
         (pages (man-completing--list-manpages))
         (sym (thing-at-point 'symbol))
         (matching-keys (cl-loop for key being the hash-keys of pages
                                 if (and sym (string-match-p (rx-to-string `(and bos ,sym ".") t) key))
                                 collect key))
         (key (if (equal 1 (length matching-keys))
                  (car matching-keys)
                (let ((choices (sort (or matching-keys (hash-table-keys pages)) #'string<)))
                  (completing-read "Manual entry: " choices nil t sym 'Man-topic-history)))))
    (if-let* ((hit (gethash key pages)))
        (apply #'interpolate "%%section %%name" hit)
      (man-completing--read))))

(defun man-completing (man-args)
  "Replace `man' with a version that does completion."
  (interactive (list (man-completing--read)))
  (Man-getpage-in-background (Man-translate-references man-args)))

(define-minor-mode man-completing-mode
  "Use manpath for manpage completion."
  :global t
  :group 'man-completing
  (if man-completing-mode
      (advice-add 'man :override #'man-completing)
    (advice-remove 'man #'man-completing)))

(provide 'man-completing)

;;; man-completing.el ends here
