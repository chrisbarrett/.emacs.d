;;; cb-arc-darker-theme.el --- Light colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-arc-darker)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-arc-darker
       (cb-theme-common-make-theme
        :default-bg "#f3f4f5"
        :default-fg "#2f343f"
        :region-bg "lightblue"
        :subtle-bg "#e1dddd"
        :paren-match cb-theme-common-cyan
        :highlight-bg "gray82"
        :header-line-bg "#414a59"
        :error-fg "#cc575d"
        :emphasis-1 cb-theme-common-blue
        :emphasis-2 "white"
        :pending-bg "#FFFFAA"))

(provide-theme 'cb-arc-darker)

;;; cb-arc-darker-theme.el ends here
