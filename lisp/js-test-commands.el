;;; js-test-commands.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'f)
(require 's)
(require 'subr-x)

(autoload 'projectile-project-buffer-p "projectile")
(autoload 'projectile-project-p "projectile")
(autoload 'projectile-project-root "projectile")
(autoload 'projectile-run-compilation "projectile")
(autoload 'projectile-test-file-p "projectile")

(defvar js-test-commands--format-test-command-pattern "jest %s --coverage"
  "A templated shell command string used to run tests.

The following template variables are processed:

- The strings \"%s\" and \"%{file}\" are replaced with the
  filename of the test.

- The string %{desc} is replaced with the description from the
  outermost describe, test or it.")

(put 'js-test-commands--format-test-command-pattern 'safe-local-variable 'stringp)

(defvar js-test-commands-find-test-function #'ignore
  "A function to find a test file name for the given impl file path.

If the function cannot find the file, it should return nil.")

(put 'js-test-commands--format-test-command-pattern 'safe-local-variable 'functionp)



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

(defun js-test-commands--test-file-in-test-tree-p (impl-file style)
  (when-let* ((guess (js-test-commands--impl-to-test-file-in-test-tree impl-file)))
    (cond
     ((file-exists-p guess)
      guess)
     ((seq-intersection '("index.js" "index.ts") (list (file-name-nondirectory impl-file)))
      (let ((dir (file-name-directory (directory-file-name (file-name-directory guess))))
            (base (file-name-base (directory-file-name (file-name-directory guess))))
            (ext (file-name-extension guess)))
        (f-join dir
                (if (equal style 'colocate)
                    (format "%s.test.%s" base ext)
                  (format "%s.%s" base ext)))))
     ((seq-intersection '("js" "ts") (file-name-extension guess))
      (let ((dir (file-name-directory guess))
            (base (file-name-nondirectory (file-name-sans-extension guess)))
            (ext (file-name-extension guess)))
        (if (equal style 'colocate)
            (format "%s.test.%s" base ext)
          (format "%s.%s" base ext)))))))

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

(defun js-test-commands-locate-test-file (impl-file &optional must-exist-p)
  (let ((style (js-test-commands--guess-test-style impl-file)))
    (or (js-test-commands--test-file-in-same-dir-p impl-file)
        (js-test-commands--test-file-in-test-tree-p impl-file style)
        (funcall js-test-commands-find-test-function)
        (unless must-exist-p
          (js-test-commands--new-test-file-path impl-file)))))

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

(defun js-test-commands--format-desc-regex (descs)
  (shell-quote-argument (string-join descs "|")))

(defun js-test-commands--format-test-command (&optional file descs)
  (s-replace-all `(("%s" . ,(shell-quote-argument file))
                   ("%{file}" . ,(shell-quote-argument file))
                   ("%{desc}" . ,(js-test-commands--format-desc-regex descs)))
                 js-test-commands--format-test-command-pattern))

(defconst js-test-commands--match-test-suite
  (rx bol (or "describe" "test" "it") symbol-end (* space)
      ;; Start of bracket group
      "(" (* space)
      ;; Start of string
      (any "\"" "'" "`")
      ;; String content
      (group (+? any))
      ;; String close.
      (any "\"" "'" "`") (* space) ","))

(defun js-test-commands--find-test-descs (file)
  (seq-map #'cadr (s-match-strings-all js-test-commands--match-test-suite (f-read file 'utf-8))))

(defun js-test-commands-test-this-file-dwim (file)
  "Run test suite corresponding to FILE."
  (interactive (list
                (if (projectile-test-file-p (buffer-file-name))
                    (buffer-file-name)
                  (js-test-commands-locate-test-file (buffer-file-name) t))))
  (let* ((descs (when file (js-test-commands--find-test-descs file)))
         (project-root (projectile-project-root))
         (command (js-test-commands--format-test-command file descs))
         (compilation-buffer-name-function (lambda (&rest _) "*projectile-test*"))
         (default-directory project-root))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      project-root)))
    (projectile-run-compilation command)))

(provide 'js-test-commands)

;;; js-test-commands.el ends here
