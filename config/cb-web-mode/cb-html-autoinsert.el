;;; cb-html-autoinsert.el --- Auto insert config for html modes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(autoload 'yas-expand-snippet "yasnippet")
(autoload 's-trim "s")

(defconst cb-html-autoinsert--snippet "

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

(defun cb-html-autoinsert-template-string ()
  (yas-expand-snippet (s-trim cb-html-autoinsert--snippet)))

(provide 'cb-html-autoinsert)

;;; cb-html-autoinsert.el ends here
