;;; cb-native-themeing.el --- <enter description here>  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defun cb-themeing-gtk ()
  "Select a theme setting depending on the current OS theme."
  (with-temp-buffer
    (ignore-errors
      (call-process "gsettings" nil t nil
                    "get" "org.gnome.desktop.interface" "gtk-theme"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

(defun cb-themeing-macos ()
  (with-temp-buffer
    (ignore-errors
      (call-process "defaults" nil t nil
                    "read" "-g" "AppleInterfaceStyle"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

(cl-defun cb-theme-for-system-type (&key light dark)
  (let ((theme
         (pcase system-type
           ('gnu/linux (cb-themeing-gtk))
           ('darwin (cb-themeing-macos)))))
    (if (equal 'dark theme)
        dark
      light)))



;;; Set background colour based on OS theme

;; Set reasonable placeholder foreground and background colours until the theme
;; is loaded. Use the current WM theme to determine whether to use light or dark
;; colours.

(set-background-color (cb-theme-for-system-type :dark "#282c34" :light "#FDF6E3"))
(set-foreground-color (cb-theme-for-system-type :dark "#bbc2cf" :light "#556b72"))

(provide 'cb-native-themeing)

;;; cb-native-themeing.el ends here
