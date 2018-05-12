;;; ensime-flycheck-integration.el --- Provide a uniform interface for combining ensime and flycheck.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

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

(require 'ensime)
(require 'flycheck)
(require 'dash)

(autoload 'evil-define-key "evil")

(defun ensime-flycheck-integration--ensime-buffer-notes ()
  (when (ensime-connected-p)
    (let ((conn (ensime-connection)))
      (append (ensime-java-compiler-notes conn)
              (ensime-scala-compiler-notes conn)))))

(defun ensime-flycheck-integration--next-ensime-note ()
  (-let* ((start (point))
          (notes (ensime-flycheck-integration--ensime-buffer-notes))
          ((&plist :beg beg) (ensime-next-note-in-current-buffer notes t))
          (end (if beg (ensime-internalize-offset beg) (point-min))))
    (when (< start end)
      end)))

(defun ensime-flycheck-integration--prev-ensime-note ()
  (-let* ((start (point))
          (notes (ensime-flycheck-integration--ensime-buffer-notes))
          ((&plist :beg beg) (ensime-next-note-in-current-buffer notes nil))
          (end (if beg (ensime-internalize-offset beg) (point-max))))
    (when (> start end)
      end)))

(defun ensime-flycheck-integration--next-flycheck-note ()
  (flycheck-next-error-pos 1))

(defun ensime-flycheck-integration--prev-flycheck-note ()
  (flycheck-next-error-pos -1))

(defun ensime-flycheck-integration--ensime-notes-at-point ()
  (-filter (-lambda ((&plist :beg beg :end end))
             (<= beg (point) (1+ end)))
           (ensime-flycheck-integration--ensime-buffer-notes)))

(defun ensime-flycheck-integration--display-errors-at-pt ()
  (when-let (errors
             (-map (-lambda ((&plist :msg msg :severity level :line line :col col))
                     (flycheck-error-new
                      :id ""
                      :buffer (current-buffer)
                      :checker nil
                      :filename (buffer-file-name)
                      :line line
                      :column col
                      :message msg
                      :level level))
                   (ensime-flycheck-integration--ensime-notes-at-point)))
    (funcall flycheck-display-errors-function errors)))

;;;###autoload
(defun ensime-flycheck-integration-next-error ()
  "Move forward to the closest Flycheck or ENSIME error."
  (interactive)
  (let* ((flycheck-pos (ensime-flycheck-integration--next-flycheck-note))
         (ensime-pos (ensime-flycheck-integration--next-ensime-note)))

    (call-interactively
     (cond
      ((and (null flycheck-pos) (null ensime-pos))
       (user-error "No more errors"))

      ((null flycheck-pos) #'ensime-forward-note)
      ((null ensime-pos) #'flycheck-next-error)
      ((< flycheck-pos ensime-pos) #'flycheck-next-error)
      (t
       #'ensime-forward-note)))

    (ensime-flycheck-integration--display-errors-at-pt)))

;;;###autoload
(defun ensime-flycheck-integration-prev-error ()
  "Move backward to the closest Flycheck or ENSIME error."
  (interactive)
  (let* ((flycheck-pos (ensime-flycheck-integration--prev-flycheck-note))
         (ensime-pos (ensime-flycheck-integration--prev-ensime-note)))
    (call-interactively
     (cond
      ((and (null flycheck-pos) (null ensime-pos))
       (user-error "No more errors"))

      ((null flycheck-pos) #'ensime-backward-note)
      ((null ensime-pos) #'flycheck-previous-error)
      ((< flycheck-pos ensime-pos) #'ensime-backward-note)
      (t
       #'flycheck-previous-error)))

    (ensime-flycheck-integration--display-errors-at-pt)))

(provide 'ensime-flycheck-integration)

;;; ensime-flycheck-integration.el ends here
