;;; info-path-from-nix.el --- Store a version of the info path for every Emacs version we build  -*- lexical-binding: t; -*-

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

;; Populate the `info' database with manuals for packages installed via Nix.

;; Example configuration:
;;
;;    (use-package info-path-from-nix
;;      :after info
;;      :demand t
;;      :config
;;      (setq Info-directory-list (append Info-default-directory-list
;;                                        (info-path-from-nix))))

;;; Code:

(require 'f)
(require 'info)
(require 'persist)
(require 's)

(defconst info-path-from-nix--emacs-nix-hash
  (or (cadr (s-match (rx "/nix/store/" (group (+? nonl)) "-")
                     invocation-directory))
      "current"))

(defconst info-path-from-nix-variable-name
  (intern (format "info-path-from-nix--%s" info-path-from-nix--emacs-nix-hash)))

(eval
 `(persist-defvar ,info-path-from-nix-variable-name nil
                  "Additional paths to add to the info path.

Don't access this variable directly. Instead, use
`info-path-from-nix'."))

(defun info-path-from-nix--persisted-value ()
  (eval info-path-from-nix-variable-name))

(defun info-path-from-nix--paths-from-load-path ()
  (seq-filter (lambda (path)
                (and (f-dir-p path)
                     (f-files path (lambda (file) (f-ext-p file "info")))))
              load-path))

(defun info-path-from-nix ()
  (unless (info-path-from-nix--persisted-value)
    (set info-path-from-nix-variable-name (info-path-from-nix--paths-from-load-path)))
  (info-path-from-nix--persisted-value))

(provide 'info-path-from-nix)

;;; info-path-from-nix.el ends here
