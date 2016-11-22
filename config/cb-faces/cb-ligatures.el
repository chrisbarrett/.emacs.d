;;; cb-ligatures.el --- Use Hasklig to provide ligatures.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>
;; Package-Requires: ((dash "2.12.1"))

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

(autoload '-union "dash")

(defconst cb-ligatures-alist
  '(("&&" . #XE100)
    ("***" . #XE101)
    ("*>" . #XE102)
    ("\\\\" . #XE103)
    ("||" . #XE104)
    ("|>" . #XE105)
    ("::" . #XE106)
    ("==" . #XE107)
    ("===" . #XE108)
    ("==>" . #XE109)
    ("=>" . #XE10A)
    ("=<<" . #XE10B)
    ("!!" . #XE10C)
    (">>" . #XE10D)
    (">>=" . #XE10E)
    (">>>" . #XE10F)
    (">>-" . #XE110)
    (">-" . #XE111)
    ("->" . #XE112)
    ("-<" . #XE113)
    ("-<<" . #XE114)
    ("<*" . #XE115)
    ("<*>" . #XE116)
    ("<|" . #XE117)
    ("<|>" . #XE118)
    ("<$>" . #XE119)
    ("<>" . #XE11A)
    ("<-" . #XE11B)
    ("<<" . #XE11C)
    ("<<<" . #XE11D)
    ("<+>" . #XE11E)
    (".." . #XE11F)
    ("..." . #XE120)
    ("++" . #XE121)
    ("+++" . #XE122)
    ("/=" . #XE123)))

;;;###autoload
(defun cb-ligatures-init ()
  (when (equal 'Hasklig (ignore-errors (font-get (face-attribute 'default :font) :family)))
    (setq prettify-symbols-alist (-union prettify-symbols-alist cb-ligatures-alist))
    ;; Refresh symbol rendering.
    (prettify-symbols-mode -1)
    (prettify-symbols-mode +1)))

(provide 'cb-ligatures)

;;; cb-ligatures.el ends here
