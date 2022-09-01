;;; ol-dotnet-src.el --- Org link type for dotnet sources  -*- lexical-binding: t; -*-

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

;; Link Syntax: 'dotnet-src:' [ LIB '#' ] PATH [ '::' ( line-no | search-string )]
;;
;; e.g. dotnet-src:System/String.CoreCLR.cs
;; e.g. dotnet-src:System.Private.CoreLib#System/String.CoreCLR.cs
;; e.g. dotnet-src:System.Private.CoreLib#System/String.CoreCLR.cs::Intern

;;; Code:

(require 'dash)
(require 'ol)
(require 's)

(defgroup ol-dotnet-src nil
  "Org link type for looking up dotnet source files.

Uses a local checkout of the dotnet runtime when available.
Otherwise, looks up online using source.dot.net."
  :group 'productivity
  :prefix "ol-dotnet-src-")

(defcustom ol-dotnet-src-runtime-location nil
  "Path to the local checkout of the dotnet runtime.

If nil, always look up online."
  :group 'ol-dotnet-src
  :type '(choice (directory :tag "Filepath")
                 (const :tag "None" nil)))

(defcustom ol-dotnet-default-lib "System.Private.CoreLib"
  "The default lib to look up if none is specified in the link."
  :group 'ol-dotnet-src
  :type 'string)



(defun ol-dotnet-lookup-online (lib file ref)
  (browse-url (format "https://source.dot.net/#%s/src/%s%s"
                      lib
                      file
                      (pcase ref
                        ((and (pred stringp)
                              (pred string-to-number))

                         (concat "," ref))
                        (_ "")))))

(defun ol-dotnet-lookup-locally (lib file &optional ref)
  (when-let* ((dir (pcase lib
                     ("System.Private.CoreLib"
                      (expand-file-name "src/coreclr/System.Private.CoreLib/src/"
                                        ol-dotnet-src-runtime-location))))
              (filepath (expand-file-name file dir)))
    (when (file-exists-p filepath)
      (find-file filepath)
      (let ((line (and (stringp ref)
                       (string-to-number ref))))
        (cond
         ((and line (cl-plusp line))
          (goto-char (point-min))
          (forward-line (1- line))
          (back-to-indentation))
         ((stringp ref)
          (goto-char (point-min))
          (search-forward ref)
          (back-to-indentation))))
      t)))

(defun ol-dotnet-follow-dotnet-source (str &optional _arg)
  (-let* ((parts (split-string str "#"))
          (lib (if (cdr parts)
                   (car parts)
                 ol-dotnet-default-lib))
          ((file ref) (-some->> (-last-item parts)
                        (s-split "::"))))
    (or (ol-dotnet-lookup-locally lib file ref)
        (ol-dotnet-lookup-online lib file ref))))

(org-link-set-parameters "dotnet-src" :follow #'ol-dotnet-follow-dotnet-source)

(provide 'ol-dotnet-src)

;;; ol-dotnet-src.el ends here
