;;; cb-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package hidden-mode-line
  :disabled t
  :config
  (global-hidden-mode-line-mode))

;; Turn the modeline into a thin grey strip.

(setq-default mode-line-format "")

(custom-set-faces
 `(mode-line
   ((t (:foreground "white" :background "gray40" :height 20)))))

(provide 'cb-modeline)

;;; cb-modeline.el ends here
