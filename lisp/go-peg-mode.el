;;; go-peg-mode.el --- Emacs mode for editing Go PEG grammar files.  -*- lexical-binding: t; -*-

;; Copyright (C) 2008  Utz-Uwe Haus <lisp@uuhaus.de>
;; Copyright (C) 2017  Chris Barrett <chris+emacs@walrus.cool>

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

;; This file provides a major mode for editing PEG grammar files, using the Go
;; tool syntax.
;;
;; See:
;;
;;   https://github.com/pointlander/peg
;;
;; It is based on Utz-Uwe Haus' code, here:
;;
;;   https://www.emacswiki.org/emacs/peg.el
;;
;; The indentation mode is adapted from go-peg-mode--mode.
;;
;;   https://github.com/go-peg-mode--lang/go-peg-mode--mode

;;; Code:

(require 'go-mode)

(defgroup go-peg-mode nil
  "Major mode for editing Go PEG files."
  :group 'languages
  :prefix "go-peg-mode-")

(defface go-peg-mode-production-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face for the names of parser productions in PEG buffers."
  :group 'go-peg-mode)


;; Mode definition

(defvar go-peg-mode-syntax-table
  (let ((st (make-syntax-table go-mode-syntax-table)))
    (modify-syntax-entry ?#  "<"  st)
    (modify-syntax-entry ?*  "."  st)
    (modify-syntax-entry ?/  "."  st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\[ "|"  st)
    (modify-syntax-entry ?\] "|"  st)
    (modify-syntax-entry ?'  "|"  st)
    st)
  "Syntax table used in `go-peg-mode' buffers.")

;;;###autoload
(define-derived-mode go-peg-mode go-mode "Go Peg"
  "Major mode for Go PEG files.

When started, runs `go-peg-mode-hook'.

\\{go-peg-mode-map}"
  :syntax-table go-peg-mode-syntax-table
  (setq-local comment-start "#"))

(font-lock-add-keywords
 'go-peg-mode
 `((,(rx (group (+ (any graph)))
         (* (any space "\t"))
         "<-")
    1 'go-peg-mode-production-name)))

(provide 'go-peg-mode)

;;; go-peg-mode.el ends here
