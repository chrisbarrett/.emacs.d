;;; parameters.el --- Shared configuration variables.  -*- lexical-binding: t; -*-
;;; Commentary:

;; These settings are applied before themes are loaded. They can be overridden
;; in ~/.config/hostfile.el.

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

(defvar parameters-variable-pitch-text-height 90
  "The default text height for variable-pitch. Bump this value for HIDPI screens.")

(provide 'parameters)

;;; parameters.el ends here
