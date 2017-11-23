;;; cb-scala-script-mode.el --- Major mode for scala script files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(autoload 'scala-mode "scala-mode")

;;;###autoload
(defvar cb-scala-script-mode-map
  (make-sparse-keymap)
  "Keymap for `cb-scala-script-mode'.")

;;;###autoload
(define-derived-mode cb-scala-script-mode scala-mode "SBT File"
  "Major mode for editing SBT files.

\\{cb-scala-script-mode-map}")

(provide 'cb-scala-script-mode)

;;; cb-scala-script-mode.el ends here
