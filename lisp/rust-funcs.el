;;; rust-funcs.el --- Supporting functions for Rust config  -*- lexical-binding: t; -*-

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

(require 'rust-mode)

(defun rust-funcs-toggle-pub ()
  "Toggle the `pub' access modifier for the decl at point."
  (interactive)
  (save-match-data
    (save-excursion
      (back-to-indentation)
      (unless (save-excursion
                (looking-at rust-top-item-beg-re))
        (beginning-of-defun)
        (back-to-indentation))
      (cond ((looking-at (rx "pub" symbol-end (* space)))
             (delete-region (match-beginning 0) (match-end 0))
             (message "internal"))
            ((save-excursion
               (back-to-indentation)
               (looking-at rust-top-item-beg-re))
             (goto-char (match-beginning 0))
             (insert "pub")
             (just-one-space)
             (message "public")
             )
            (t
             (user-error "Cannot toggle pub modifier here"))))))

(provide 'rust-funcs)

;;; rust-funcs.el ends here
