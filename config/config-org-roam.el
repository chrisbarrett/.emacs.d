;;; config-org-roam.el --- Configuration specifically for org-roam and related packages  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'use-package)
  (require 'org))

(require 'dash)
(require 'f)
(require 'memoize)
(require 's)

;; Use rg to search for candidates for org-agenda-files

(defun config-org-roam--org-files-with-todos (dir)
  (with-temp-buffer
    (let ((default-directory dir))
      (call-process "rg" nil t nil
                    "TODO|CANCELLED|WAIT|DONE"
                    "--files-with-matches"
                    "--type" "org"
                    "--case-sensitive"
                    "--max-depth" "1")
      (let ((results (split-string (buffer-substring (point-min) (point-max)) "\n" t)))
        (seq-map (lambda (it) (f-join dir it))
                 results)))))

(ignore-errors
  (memoize #'config-org-roam--org-files-with-todos "1 minute"))

(defun config-org-roam--find-org-files-with-todos ()
  (let* ((dirs (-flatten (list org-directory
                               (when (bound-and-true-p org-roam-directory)
                                 (list org-roam-directory
                                       (f-join org-roam-directory "dailies")
                                       (f-join org-roam-directory "projects"))))))
         (files (seq-mapcat #'config-org-roam--org-files-with-todos (seq-filter #'identity dirs))))
    (seq-filter (lambda (file)
                  ;; ignore dropbox conflict files
                  (not (s-contains-p "conflicted copy" (f-filename file))))
                files)))

(defun config-org-roam--update-agenda-files (&rest _)
  (setq org-agenda-files (config-org-roam--find-org-files-with-todos)))

(add-hook 'org-roam-mode-hook #'config-org-roam--update-agenda-files)

;; Populate agenda with files from the roam directory.
;;
;; Improve agenda performance by only including files that have todos.
;; Otherwise, org-agenda would attempt to load every org-roam file.

(advice-add 'org-agenda :before #'config-org-roam--update-agenda-files)

;; `org-roam' provides a specialised orgmode workflow and backlinks.
(use-package org-roam
  :hook (after-init . org-roam-mode)
  :general
  (:states '(insert normal) :keymap 'org-mode-map
   "C-c i" 'org-roam-insert
   "C-c C-i" 'org-roam-insert
   "C-c I" 'org-roam-insert-immediate)
  :custom
  ((org-roam-directory (f-join paths-org-directory "roam"))
   (org-roam-db-location (f-join paths-cache-directory "org-roam.db"))))

;; `company-org-roam' provides a company backend for org-roam topics.
(use-package company-org-roam
  :after (:all org-roam company)
  :config
  (push 'company-org-roam company-backends))

;; `deft' provides a fast note-taking and search UI.

(use-package deft
  :commands (deft)
  :preface
  (defun config-org-roam--kill-deft ()
    (interactive)
    (when deft-buffer
      (kill-buffer deft-buffer)))

  :general
  (:states 'emacs :keymaps 'deft-mode-map
   "C-g" #'config-org-roam--kill-deft
   "<escape>" #'config-org-roam--kill-deft)
  :custom
  ((deft-directory org-roam-directory)
   (deft-default-extension "org")
   (deft-use-filter-string-for-filename t)
   (deft-auto-save-interval 0)
   (deft-file-naming-rules '((noslash . "-")
                             (nospace . "-")
                             (case-fn . downcase))))
  :config
  (add-to-list 'deft-extensions "tex"))


(provide 'config-org-roam)

;;; config-org-roam.el ends here
