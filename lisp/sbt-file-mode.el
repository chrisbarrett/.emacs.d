;;; sbt-file-mode.el --- Major mode for scala script files  -*- lexical-binding: t; -*-

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

(provide 'sbt-file-mode)

;;; sbt-file-mode.el ends here
