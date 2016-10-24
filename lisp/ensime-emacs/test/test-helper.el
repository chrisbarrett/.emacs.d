;;; test-helper.el --- ensime-emacs: ERT Test helper

;; Copyright (C) 2015 ENSIME authors
;; License: http://www.gnu.org/licenses/gpl.html

(require 'f)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;
;; variables
;;;;;;;;;;;;;;;;;;;;

(defvar ensime-emacs-test/test-path
  (f-dirname (f-this-file))
  "Path to tests directory.")

(defvar ensime-emacs-test/root-path
  (f-parent ensime-emacs-test/test-path)
  "Path to root directory.")

(defvar ensime-emacs-test/resource-path
  (f-expand "resources" ensime-emacs-test/test-path)
  "Path to 'resources' directory files.")

;;;;;;;;;;;;;;;;;;;;
;; function helpers
;;;;;;;;;;;;;;;;;;;;

(defun ensime-emacs-test-resource-filename (resource-file)
  "Determine the absolute file name of a RESOURCE-FILE.
Relative file names are expanded against
`flycheck-ert-resources-directory'."
  (f-expand resource-file ensime-emacs-test/resource-path))

(defun ensime-create-file (file-name contents)
  "Create file named file-name. Write contents to the file. Return file's name."
  (make-directory (file-name-directory file-name) t)
  (with-temp-file file-name
    (insert contents))
  file-name)

(add-to-list 'load-path ensime-emacs-test/root-path)
(add-to-list 'load-path ensime-emacs-test/test-path)

(require 'ensime)
