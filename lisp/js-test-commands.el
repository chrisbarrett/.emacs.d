;;; js-test-commands.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'f)
(require 's)

(autoload 'projectile-project-buffer-p "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'projectile-project-root "projectile")
(autoload 'projectile-run-compilation "projectile")
(autoload 'projectile-test-file-p "projectile")

;; Silence byte-compiler warning.
(eval-when-compile
  (defvar projectile-test-cmd-map (make-hash-table :test 'equal)))

(defun js-test-commands--impl-to-colocated-test-file (impl-file)
  (format "%s.test.%s" (f-no-ext impl-file) (f-ext impl-file)))

(defun js-test-commands--impl-to-test-file-in-test-tree (impl-file)
  (replace-regexp-in-string (rx "/" (or "lib" "src") "/") "/test/" impl-file t t))

(defun js-test-commands--test-file-in-same-dir-p (impl-file)
  (let ((guess (js-test-commands--impl-to-colocated-test-file impl-file)))
    (when (file-exists-p guess)
      guess)))

(defun js-test-commands--test-file-in-test-tree-p (impl-file)
  (when-let* ((guess (js-test-commands--impl-to-test-file-in-test-tree impl-file)))
    (cond
     ((file-exists-p guess)
      guess)
     ((seq-intersection '("index.js" "index.ts") (file-name-nondirectory impl-file))
      (let ((dir (file-name-directory (directory-file-name (file-name-directory guess))))
            (base (file-name-base (directory-file-name (file-name-directory guess))))
            (ext (file-name-extension guess)))
        (f-join dir (format "%s.test.%s" base ext))))
     ((seq-intersection '("js" "ts") (file-name-extension guess))
      (let ((dir (file-name-directory guess))
            (base (file-name-nondirectory (file-name-sans-extension guess)))
            (ext (file-name-extension guess)))
        (f-join dir (format "%s.test.%s" base ext)))))))

(defun js-test-commands--guess-test-style (file)
  (if (locate-dominating-file file "test")
      'test-dir
    'colocate))

(defun js-test-commands--new-test-file-path (impl-file)
  (pcase (js-test-commands--guess-test-style impl-file)
    ('colocate
     (js-test-commands--impl-to-colocated-test-file impl-file))
    ('test-dir
     (js-test-commands--impl-to-test-file-in-test-tree impl-file))
    (_
     (error "Unable to determine what test style this project should use"))))

(defun js-test-commands-locate-test-file (impl-file)
  (or (js-test-commands--test-file-in-same-dir-p impl-file)
      (js-test-commands--test-file-in-test-tree-p impl-file)
      (js-test-commands--new-test-file-path impl-file)))

(defun js-test-commands--impl-file-in-same-dir-p (test-file)
  (let ((re (rx (group ".test") "." (or "js" "ts") eos)))
    (when (string-match-p re  test-file)
      (s-replace-regexp re "" test-file nil nil 1))))

(defun js-test-commands--impl-file-in-src-tree-p (test-file)
  (let* ((impl-dir (if (f-dir? (f-join (projectile-project-root) "lib"))
                       "/lib/"
                     "/src/"))
         (guess (s-replace-all `((".test" . "")
                                 ("/test/" . ,impl-dir))
                               test-file)))
    (if (file-directory-p (f-no-ext guess))
        (f-join (f-no-ext guess) "index.%s" (f-ext guess))
      guess)))

(defun js-test-commands-locate-impl-file (test-file)
  (or (js-test-commands--impl-file-in-same-dir-p test-file)
      (js-test-commands--impl-file-in-src-tree-p test-file)))

(defun js-test-commands-test-this-file-dwim (file)
  "Run test suite corresponding to FILE."
  (interactive (list
                (if (projectile-test-file-p (buffer-file-name))
                    (buffer-file-name)
                  (js-test-commands-locate-test-file (buffer-file-name)))))
  (let ((command (format "jest %s" (shell-quote-argument file)))
        (project-root (projectile-project-root))
        (compilation-buffer-name-function (lambda (&rest _) "*projectile-test*")))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      project-root)))
    (projectile-run-compilation command)))

(provide 'js-test-commands)

;;; js-test-commands.el ends here
