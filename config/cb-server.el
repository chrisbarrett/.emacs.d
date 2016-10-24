;;; cb-server.el --- Configure Emacs server.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'server)

(unless (server-running-p)
  (server-start))

(provide 'cb-server)

;;; cb-server.el ends here
