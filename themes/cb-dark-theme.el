;;; cb-dark-theme.el --- Dark colour theme. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(deftheme cb-dark)

(require 'cb-theme-common)

(defconst cb-dark-theme-orange "#de935f")
(defconst cb-dark-theme-red "#cc6666")

(apply #'custom-theme-set-faces 'cb-dark
       (cb-theme-common-make-theme
        :default-fg "#c5c8c6"
        :default-bg "#1d1f21"
        :region-bg "#373b41"
        :subtle-bg "#22a224a427a7"
        :dimmed-fg "#555856"
        :paren-match "white"
        :emphasis cb-dark-theme-orange
        :highlight-bg "#3c3a3a"
        :error-fg cb-dark-theme-red
        :mode-line-fg "#c5c8c6"
        :mode-line-bg "#373b41"))

(provide-theme 'cb-dark)

;;; cb-dark-theme.el ends here
