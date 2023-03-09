;;; project-funcs.el --- Helper functions for project.el  -*- lexical-binding: t; -*-

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

(require 'subr-x)

(defgroup project-funcs nil
  "Extra functionality for projects."
  :group 'tools
  :prefix "project-funcs-")

(defcustom project-funcs-known-files '("Cargo.toml"
                                       "package.json"
                                       "flake.nix"
                                       "shell.nix")
  "Files that suggest a project root."
  :group 'project-funcs
  :type '(list string))

(defun project-funcs-try-known-files (start-dir)
  (when-let* ((dir (and (file-directory-p start-dir)
                        (locate-dominating-file start-dir
                                                (lambda (dir)
                                                  (seq-intersection project-funcs-known-files (directory-files dir)))))))
    ;; KLUDGE: use the default vc implementation.
    (cons 'vc dir)))

(provide 'project-funcs)

;;; project-funcs.el ends here
