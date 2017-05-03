;;; cb-light-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-light)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-light
       (cb-theme-common-make-theme "black" "grey90"))

(provide-theme 'cb-light)

;;; cb-light-theme.el ends here
