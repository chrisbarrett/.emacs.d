;;; org-latex-themed-previews.el --- Make latex previews work better with theme switching  -*- lexical-binding: t; -*-

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

;; Teach org-latex previews to create images that fit with the current theme.
;; This works by always using transparent backgrrounds and using the default
;; face's foreground color.

;; Inspired by: https://stackoverflow.com/a/59092163

;;; Code:

(require 'f)
(require 'org)

(defgroup org-latex-themed-previews nil
  "Make latex previews work better with theme switching."
  :group 'org
  :prefix "org-latex-themed-previews-")



(defvar org-latex-themed-previews--original-dir org-preview-latex-image-directory)
(defvar org-latex-themed-previews--original-format-options org-format-latex-options)

(defun org-latex-themed-previews--apply-vars (&rest _)
  (let*  ((dirname  (concat "bg-"
                            (string-join (seq-map #'int-to-string (color-values (face-background 'default))) "-")
                            "--fg-"
                            (string-join (seq-map #'int-to-string (color-values (face-foreground 'default))) "-")
                            "/")))
    (setq org-preview-latex-image-directory (f-join org-latex-themed-previews--original-dir dirname))
    (plist-put org-format-latex-options :background 'default)
    (plist-put org-format-latex-options :foreground 'default)))

(defsubst org-latex-themed-previews--restore-vars ()
  (setq org-preview-latex-image-directory org-latex-themed-previews--original-dir)
  (setq org-format-latex-options org-latex-themed-previews--original-format-options))



(defun org-latex-themed-previews--rebuild-fragments (&rest _)
  (org-latex-themed-previews--apply-vars)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (get-buffer-window-list buf) (derived-mode-p 'org-mode))
        (org-clear-latex-preview (point-min) (point-max))
        (org-latex-preview '(16))))))

(define-minor-mode org-latex-themed-previews-mode
  "Minor mode for integrating latex previews with color themes."
  :group 'org-latex-themed-previews
  :global t
  (if org-latex-themed-previews-mode
      (progn
        (org-latex-themed-previews--apply-vars)
        (advice-add 'org-toggle-latex-fragment :before #'org-latex-themed-previews--apply-vars)
        (advice-add 'org-latex-preview :before #'org-latex-themed-previews--apply-vars)
        (advice-add 'load-theme :after #'org-latex-themed-previews--rebuild-fragments)
        (advice-add 'disable-theme :after #'org-latex-themed-previews--rebuild-fragments))
    (org-latex-themed-previews--restore-vars)
    (advice-remove 'org-toggle-latex-fragment #'org-latex-themed-previews--apply-vars)
    (advice-remove 'org-latex-preview #'org-latex-themed-previews--apply-vars)
    (advice-remove 'load-theme #'org-latex-themed-previews--rebuild-fragments)
    (advice-remove 'disable-theme #'org-latex-themed-previews--rebuild-fragments)))

(provide 'org-latex-themed-previews)

;;; org-latex-themed-previews.el ends here
