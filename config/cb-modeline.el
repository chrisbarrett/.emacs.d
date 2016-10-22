;;; cb-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package hidden-mode-line
  :config
  (global-hidden-mode-line-mode))

(custom-set-faces
 `(mode-line
   ((t (:foreground "white" :background "gray40")))))

(provide 'cb-modeline)

;;; cb-modeline.el ends here
