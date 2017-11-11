;;; cb-arc-dark-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-arc-dark)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-arc-dark
       (cb-theme-common-make-theme
        :default-fg "#f3f4f5"
        :default-bg "#2c303b"
        :region-bg "lightblue"
        :subtle-bg "#2f343f"
        :paren-match cb-theme-common-cyan
        :highlight-bg "#1a1a2c"
        :header-line-fg "#f5f6f7"
        :header-line-bg "#21242c"
        :error-fg "#cc575d"
        :emphasis-1 cb-theme-common-blue
        :emphasis-2 "white"))

(provide-theme 'cb-arc-dark)

;;; cb-arc-dark-theme.el ends here
