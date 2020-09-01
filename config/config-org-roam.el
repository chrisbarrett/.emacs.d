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

(defvar config-org-roam-bibliography-notes-directory (f-join org-directory "roam" "notes"))
(defvar config-org-roam-bibliography-path (f-join org-directory "bibliography.bib"))
(defvar config-org-roam-bibnotes-file (f-join config-org-roam-bibliography-notes-directory "index.org"))

(make-directory config-org-roam-bibliography-notes-directory t)

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
   (deft-file-naming-rules '((noslash . "_")
                             (nospace . "_")
                             (case-fn . downcase))))
  :config
  (add-to-list 'deft-extensions "tex"))

;; `helm-bibtex' provides a UI for searching the bibliography.
(use-package helm-bibtex
  :commands (helm-bibtex helm-bibtex-with-local-bibliography helm-bibtex-with-notes))

;; `bibtex-completion' is the underlying mechanism used by helm-bibtex.

(use-package bibtex-completion
  :preface
  (defconst config-org-roam--bibtex-file-template (string-trim-left "
#+TITLE: ${title}
#+ROAM_KEY: cite:${=key=}

* TODO Notes
:PROPERTIES:
:CUSTOM_ID: ${=key=}
:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
:AUTHOR: ${author-abbrev}
:JOURNAL: ${journaltitle}
:DATE: ${date}
:YEAR: ${year}
:DOI: ${doi}
:URL: ${url}
:END:
"))
  :custom
  ((bibtex-completion-notes-path config-org-roam-bibliography-notes-directory)
   (bibtex-completion-bibliography config-org-roam-bibliography-path)
   (bibtex-completion-pdf-field "file")
   (bibtex-completion-notes-template-multiple-files config-org-roam--bibtex-file-template)))

;; `org-roam-bibtex' integrates org-roam with bibtex for bibliography
;; management.

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  ((org-roam-bibtex-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
   (orb-templates
    '(("r" "ref" plain (function org-roam-capture--get-point)
       ""
       :file-name "${slug}"
       :head (string-trim-left "
#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
:NOTER_PAGE:
:END:

")
       :unnarrowed t)))))

;; `org-noter' allows you to annotate PDFs and other formats, storing the
;; annotations in an org file.

(use-package org-noter
  :after (:any org pdf-view)
  :custom
  ((org-noter-always-create-frame nil)
   (org-noter-hide-other nil)
   (org-noter-notes-search-path (list config-org-roam-bibliography-notes-directory))))

;; `org-ref' provides tooling for inserting and formatting references in from a
;; bibliography file.

(use-package org-ref
  :preface
  (defconst config-org-roam--ref-note-title-format (string-trim-left "
* TODO %y - %t
:PROPERTIES:
:CUSTOM_ID: %k
:NOTER_DOCUMENT: %F
:ROAM_KEY: cite:%k
:AUTHOR: %9a
:JOURNAL: %j
:YEAR: %y
:VOLUME: %v
:PAGES: %p
:DOI: %D
:URL: %U
:END:

"))
  :custom
  ((org-ref-completion-library 'org-ref-helm-cite)
   (org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
   (org-ref-default-bibliography (list config-org-roam-bibliography-path))
   (org-ref-bibliography-notes config-org-roam-bibnotes-file)
   (org-ref-note-title-format config-org-roam--ref-note-title-format)
   (org-ref-notes-directory config-org-roam-bibliography-notes-directory)
   (org-ref-notes-function 'orb-edit-notes))
  :config
  (progn
    (require 'org-ref-pdf)
    (require 'org-ref-url-utils)))

(provide 'config-org-roam)

;;; config-org-roam.el ends here
