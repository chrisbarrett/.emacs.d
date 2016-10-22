;;; cb-ag.el --- Configuration for ag and related utils.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package ag
  :commands ag)

(use-package wgrep-ag
  :after ag)

(provide 'cb-ag)

;;; cb-ag.el ends here
