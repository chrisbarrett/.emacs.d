;;; sbt-file-mode.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(autoload 'scala-mode "scala-mode")

;;;###autoload
(defvar sbt-file-mode-map
  (make-sparse-keymap)
  "Keymap for `sbt-file-mode'.")

;;;###autoload
(define-derived-mode sbt-file-mode scala-mode "SBT File"
  "Major mode for editing SBT files.

\\{sbt-file-mode-map}")


;;;###autoload
(defvar scala-script-mode-map
  (make-sparse-keymap)
  "Keymap for `scala-script-mode'.")

;;;###autoload
(define-derived-mode scala-script-mode scala-mode "SBT File"
  "Major mode for editing SBT files.

\\{scala-script-mode-map}")


(provide 'scala-submodes)

;;; scala-submodes.el ends here
