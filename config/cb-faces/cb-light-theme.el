;;; cb-light-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-light)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-light
       (cb-theme-common-make-theme
        :default-fg "black"
        :default-bg "grey90"
        :region-bg "lightblue"
        :subtle-bg "#e1dddd"
        :paren-match cb-theme-common-cyan
        :highlight-bg "gray82"
        :header-line-bg "#c6c6c6"
        :header-line-fg "#4d4d4c"
        :emphasis cb-theme-common-blue
        :pending-bg "#FFFFAA"))

(provide-theme 'cb-light)

;;; cb-light-theme.el ends here
