;;; cb-native-themeing.el --- <enter description here>  -*- lexical-binding: t; -*-
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

(provide 'cb-native-themeing)

;;; cb-native-themeing.el ends here
