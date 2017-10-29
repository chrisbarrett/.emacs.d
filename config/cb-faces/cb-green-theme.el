;;; cb-green-theme.el --- Green colour theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-green)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-green
       (cb-theme-common-make-theme
        :default-fg "#8a8a8a"
        :default-bg "#103030"
        :region-bg "#355"
        :dimmed-fg "#555"
        :subtle-bg "#102727"
        :highlight-bg "#244"
        :paren-match "white"
        :header-line-fg "#888"
        :header-line-bg "#2a4040"
        :error-fg "#cb4b16"
        :emphasis-1 "#577"
        :emphasis-2 "#133"))

(provide-theme 'cb-green)

;;; cb-green-theme.el ends here
