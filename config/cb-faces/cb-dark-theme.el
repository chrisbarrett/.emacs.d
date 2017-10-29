;;; cb-dark-theme.el --- Dark colour theme. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-dark)

(require 'cb-theme-common)

(apply #'custom-theme-set-faces 'cb-dark
       (cb-theme-common-make-theme
        :default-fg "#f88"
        :default-bg "#200505"
        :region-bg "#533"
        :subtle-bg "#311"
        :dimmed-fg "#a55"
        :paren-match "white"
        :emphasis-1 cb-theme-common-orange
        :emphasis-2 "black"
        :highlight-bg "#2c1a1a"
        :error-fg cb-theme-common-pink
        :header-line-fg "#f88"
        :header-line-bg "#533"))

(provide-theme 'cb-dark)

;;; cb-dark-theme.el ends here
