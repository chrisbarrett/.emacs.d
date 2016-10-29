;;; cb-dark-theme.el --- Dark colour theme. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-dark)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-dark (cb-theme-common-make-theme "#ccc" "#111"))

(provide-theme 'cb-dark)

;;; cb-dark-theme.el ends here
