;;; emacs-overlay.el --- Utilities for working with the nix emacs-overlay -*- lexical-binding: t; -*-

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

;; Provides a convenient interactive command to update the version of the
;; emacs-overlay used in this config.

;;; Code:

(require 'dash)
(require 'f)
(require 'plist)

(defgroup emacs-overlay nil
  "Utilities for working with the nix emacs-overlay."
  :group 'languages
  :prefix "emacs-overlay-")

(defcustom emacs-overlay-url "https://github.com/nix-community/emacs-overlay.git"
  "The URL of the emacs-overlay git repository."
  :group 'emacs-overlay
  :type 'stringp)



(defcustom emacs-overlay-version-file-name "emacs-overlay.json"
  "The file name to use to record the current version of the overlay used."
  :group 'emacs-overlay
  :type 'stringp)

(defun emacs-overlay-version--file ()
  (f-join user-emacs-directory emacs-overlay-version-file-name))

(plist-define emacs-overlay-info
  :required (:pkgs :lisp))

(defun emacs-overlay-info-load ()
  (apply 'emacs-overlay-info-create
         (json-parse-string (f-read-text (emacs-overlay-version--file))
                            :object-type 'plist)))

(defun emacs-overlay-info--write (info)
  (emacs-overlay-info-assert info)
  (with-temp-buffer
    (insert (json-serialize info))
    (json-pretty-print-buffer)
    (newline)
    (write-region (point-min) (point-max) (emacs-overlay-version--file))))

(defun emacs-overlay-versions-update (updates)
  (let* ((current (emacs-overlay-info-load))
         (updated (plist-merge current updates)))
    (emacs-overlay-info--write updated)))



(defun emacs-overlay--remote-head-ref ()
  (let* ((command (list "git" "ls-remote" emacs-overlay-url "HEAD"))
         (response (shell-command-to-string (mapconcat #'shell-quote-argument command " "))))
    (car (split-string response))))

(defun emacs-overlay-rebuild ()
  (let* ((default-directory user-emacs-directory))
    (compile "nix build")))

;;;###autoload
(defun emacs-overlay-update (&optional update-emacs-p)
  "Run the Nix build for this Emacs configuration.

By default, only Lisp packages are updated. With prefix arg
UPDATE-EMACS-P, also update the Emacs build."
  (interactive "P")
  (message "Getting latest overlay rev...")
  (-let ((upstream-rev (emacs-overlay--remote-head-ref))
         ((current &as &plist :lisp current-rev :pkg current-pkg-rev) (emacs-overlay-info-load)))
    (cond
     ((and update-emacs-p (equal upstream-rev current-rev)
           (equal upstream-rev current-pkg-rev))
      (message "Overlay version up-to-date at %s." (substring current-rev 0 7)))
     ((and update-emacs-p (y-or-n-p (format "Update to %s? " (substring upstream-rev 0 7))))
      (emacs-overlay-versions-update  (list :lisp upstream-rev :pkgs upstream-rev))
      (emacs-overlay-rebuild))

     ((equal upstream-rev current-rev)
      (message "Overlay version up-to-date at %s." (substring current-rev 0 7)))
     ((y-or-n-p (format "Update to %s? " (substring upstream-rev 0 7)))
      (emacs-overlay-versions-update (list :lisp upstream-rev))
      (emacs-overlay-rebuild))
     (t
      (message "emacs-overlay update aborted.")))))

(provide 'emacs-overlay)

;;; emacs-overlay.el ends here
