;;; org-version.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;; Straight cannot install org correctly, since it does not run make and
;; therefore does not create the org-version file.

;;; Code:

(require 'straight)
(require 'subr-x)

(straight-use-package 'git)
(require 'git)

(defun org-git-version ()
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;;; org-version.el ends here
