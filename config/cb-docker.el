;;; cb-docker.el --- Configuration for docker support.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(provide 'cb-docker)

;;; cb-docker.el ends here
