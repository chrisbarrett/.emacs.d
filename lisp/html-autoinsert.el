;;; html-autoinsert.el --- Auto insert config for html modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'subr-x)

(autoload 'yas-expand-snippet "yasnippet")

(defconst html-autoinsert--snippet "

<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <title>$0</title>
  </head>
  <body>
  </body>
</html>

")

(defun html-autoinsert-template-string ()
  (yas-expand-snippet (string-trim html-autoinsert--snippet)))

(provide 'html-autoinsert)

;;; html-autoinsert.el ends here
