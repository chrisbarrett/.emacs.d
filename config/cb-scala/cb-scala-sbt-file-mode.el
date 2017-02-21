;;; cb-scala-sbt-file-mode.el --- Major mode for .sbt files  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(autoload 'scala-mode "scala-mode")

;;;###autoload
(defvar cb-scala-sbt-file-mode-map
  (make-sparse-keymap)
  "Keymap for `cb-scala-sbt-file-mode'.")

;;;###autoload
(define-derived-mode cb-scala-sbt-file-mode scala-mode "SBT File"
  "Major mode for editing SBT files.

\\{cb-scala-sbt-file-mode-map}")

(provide 'cb-scala-sbt-file-mode)

;;; cb-scala-sbt-file-mode.el ends here
