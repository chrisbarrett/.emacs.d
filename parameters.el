;;; parameters.el --- Shared configuration variables.  -*- lexical-binding: t; -*-
;;; Commentary:

;; These settings are applied before themes are loaded. They can be overridden
;; in ~/.config/hostfile.el.

;;; Code:

(require 'f)

(defvar parameters-default-theme
  (if (equal (ignore-errors
               (string-trim (f-read-text "~/.cache/theme")))
             "light")
      'doom-solarized-light
    'doom-one)
  "The theme to use when Emacs first starts up.")

(defvar parameters-default-text-height 100
  "The default text height. Bump this value for HIDPI screens.")

(defvar parameters-default-font-family "Ubuntu Mono"
  "The font family to use for the default face.")

(defvar parameters-variable-pitch-text-height 90
  "The default text height for variable-pitch. Bump this value for HIDPI screens.")

(provide 'parameters)

;;; parameters.el ends here
