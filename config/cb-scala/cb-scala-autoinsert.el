;;; cb-scala-autoinsert.el --- Autoinsert config for Scala.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(autoload 'projectile-project-p "projectile")

(defconst cb-scala-autoinsert-form
  '((scala-mode . "Scala Src File")
    nil
    (cb-scala-autoinsert--package-name)))

(defun cb-scala-autoinsert--package-name ()
  (-if-let* ((root (and (buffer-file-name) (projectile-project-p)))
             (pkg-id (->> (f-dirname (buffer-file-name))
                          (s-chop-prefix root)
                          f-split
                          nreverse
                          (--take-while (not (-contains? '("src" "app" "scala" "test" "tests") it)))
                          nreverse
                          (s-join "."))))
      (if (s-blank? pkg-id)
          ""
        (format "package %s\n\n" pkg-id))
    ""))

(provide 'cb-scala-autoinsert)

;;; cb-scala-autoinsert.el ends here
