;;; parameters.el --- Shared configuration variables.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar parameters-default-theme
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (<= 7 hour 20)
        'doom-solarized-light
      'doom-one))
  "The theme to use when Emacs first starts up.")

(defvar parameters-default-text-height 100
  "The default text height. Bump this value for HIDPI screens.")

(defvar parameters-default-font-family "Iosevka"
  "The font family to use for the default face.")

(provide 'parameters)

;;; parameters.el ends here
