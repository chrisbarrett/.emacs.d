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

(defconst config-org-roam--notes-file-template (string-trim-left "
#+TITLE: ${title}
#+ROAM_KEY: cite:${=key=}

- tags ::
- keywords :: ${keywords}

* Notes
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

(make-directory config-org-roam-bibliography-notes-directory t)

;; Use rg to search for candidates for org-agenda-files

(defun config-org-roam--org-files-with-todos (dir)
  (with-temp-buffer
    (let ((default-directory dir))
      (call-process "rg" nil t nil
                    "^[*]+ +(TODO|WAIT)"
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
  (let ((dirs (-flatten (list org-directory
                              (when (bound-and-true-p org-roam-directory)
                                (f-join org-roam-directory "dailies"))))))
    (seq-mapcat #'config-org-roam--org-files-with-todos (seq-filter #'identity dirs))))

(defun config-org-roam--update-agenda-files (&rest _)
  (setq org-agenda-files `(,(f-join org-directory "archive.org")
                           ,(f-join org-directory "tasks")
                           ,@(config-org-roam--find-org-files-with-todos))))

(add-hook 'org-roam-mode-hook #'config-org-roam--update-agenda-files)

;; Populate agenda with files from the roam directory.
;;
;; Improve agenda performance by only including files that have todos.
;; Otherwise, org-agenda would attempt to load every org-roam file.

(advice-add 'org-agenda :before #'config-org-roam--update-agenda-files)

;; Update agenda files as we create new org buffers.

(defun config-org-roam--buffer-has-todo-keywords-p ()
  (and (derived-mode-p 'org-mode)
       (save-match-data
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp org-todo-regexp nil t)))))

(defun config-org-roam--maybe-add-to-agenda-files ()
  (when (and (derived-mode-p 'org-mode)
             (config-org-roam--buffer-has-todo-keywords-p)
             (buffer-file-name)
             (string-match-p (regexp-quote (f-join org-roam-directory "dailies"))
                             (buffer-file-name))
             (buffer-file-name))
    (add-to-list 'org-agenda-files (buffer-file-name))))

(add-hook 'org-capture-after-finalize-hook #'config-org-roam--maybe-add-to-agenda-files)
(add-hook 'after-save-hook #'config-org-roam--maybe-add-to-agenda-files)

;; `org-roam' provides a specialised orgmode workflow and backlinks.

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :general
  (:states '(insert normal) :keymap 'org-mode-map
   "C-c i" 'org-roam-insert
   "C-c C-i" 'org-roam-insert
   "C-c TAB" nil
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

;; `bibtex' and `bibtex-completion' is the underlying mechanism used by helm-bibtex.

(use-package bibtex
  :defer t
  :custom
  ((bibtex-dialect 'biblatex)
   (bibtex-completion-library-path (f-join org-directory "pdfs/"))))

(use-package bibtex-completion
  :custom
  ((bibtex-completion-notes-path config-org-roam-bibliography-notes-directory)
   (bibtex-completion-bibliography config-org-roam-bibliography-path)
   (bibtex-completion-pdf-field "file")
   (bibtex-completion-notes-template-multiple-files config-org-roam--notes-file-template)))

;; `org-roam-bibtex' integrates org-roam with bibtex for bibliography
;; management.

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :init
  (use-package orb-note-actions
    :after org-roam-bibtex
    :general (:keymaps 'org-mode-map "C-c n a" 'orb-note-actions))
  :custom
  ((org-roam-bibtex-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
   (orb-note-actions-frontend 'hydra)
   (orb-templates
    `(("r" "ref" plain (function org-roam-capture--get-point)
       ""
       :file-name "${slug}"
       :head ,config-org-roam--notes-file-template
       :unnarrowed t)))))

;; `org-noter' allows you to annotate PDFs and other formats, storing the
;; annotations in an org file.

(use-package org-noter
  :after (:any org pdf-view)
  :general (:keymaps 'pdf-view-mode-map
            :states '(normal motion)
            "i" 'org-noter-insert-note
            [?\t] 'org-noter)
  :custom
  ((org-noter-always-create-frame nil)
   (org-noter-separate-notes-from-heading t)
   (org-noter-hide-other t)
   (org-noter-insert-note-no-questions t)
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
   (org-ref-pdf-directory (f-join org-directory "pdfs/"))
   (org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
   (org-ref-default-bibliography (list config-org-roam-bibliography-path))
   (org-ref-bibliography-notes config-org-roam-bibnotes-file)
   (org-ref-note-title-format config-org-roam--ref-note-title-format)
   (org-ref-notes-directory config-org-roam-bibliography-notes-directory)
   (org-ref-notes-function 'orb-edit-notes))
  :config
  (progn
    (require 'org-ref-pdf)
    (require 'org-ref-url-utils)
    (defalias 'dnd-unescape-uri 'dnd--unescape-uri)))

(provide 'config-org-roam)

;;; config-org-roam.el ends here
