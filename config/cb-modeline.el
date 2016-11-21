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

;; Hide the modeline, but ensure the header-line still works.

(custom-set-faces
 `(mode-line
   ((t (:foreground "gray40" :background "gray40" :height 20))))
 `(header-line
   ((default
      :inherit default))))

(provide 'cb-modeline)

;;; cb-modeline.el ends here
