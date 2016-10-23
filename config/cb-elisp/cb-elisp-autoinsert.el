;;; cb-elisp-autoinsert.el --- Autoinsert configuration for Emacs Lisp. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(defconst cb-elisp-autoinsert-gnu-license "
\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
")

(defconst cb-elisp-autoinsert-form
  '((emacs-lisp-mode . "Emacs Lisp")
    nil
    "\;;; " (file-name-nondirectory (buffer-file-name)) " --- <enter description here>  "
    "-*- lexical-binding: t; -*-" '(setq lexical-binding t) \n
    \n
    ";; Copyright (C) " (format-time-string "%Y") "  "
    (getenv "ORGANIZATION") | user-full-name                \n
    \n
    ";; Author: " user-full-name " <" user-mail-address ">" \n
    cb-elisp-autoinsert-gnu-license                            \n
    ";;; Commentary:"                                       \n \n
    ";;; Code:"                                             \n \n
    _                                                       \n \n
    "\(provide '" (file-name-base) ")"                      \n \n
    "\;;; " (file-name-nondirectory (buffer-file-name)) " ends here" \n))

(provide 'cb-elisp-autoinsert)

;;; cb-elisp-autoinsert.el ends here
