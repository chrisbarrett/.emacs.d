;;; org-roam-review.el --- Extends org-roam with spaced-repetition review of notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides commands to categorise and review org-roam notes for Evergreen
;; note-taking. Notes are surfaced using the spaced-repetition algorithm from
;; org-drill.

;; The main entrypoint is `M-x org-roam-review', which shows your notes due for
;; review and refinement. With a prefix argument, that command will list all
;; your notes by category, which is useful for getting a quick overview of your
;; Evergreens.

;; Example configuration:
;;
;;     (use-package org-roam-review
;;       :hook
;;       (org-mode . org-roam-review-cache-mode)
;;       (org-roam-capture-new-node . org-roam-review-set-seedling)
;;       :custom
;;       (org-roam-review-ignored-tags '("person" "client" "project" "lit_notes"))
;;       :general
;;       ;; optional bindings for evil-mode compatability.
;;       (:states '(normal) :keymaps 'org-roam-review-mode-map
;;        "TAB" 'magit-section-cycle
;;        "g r" 'org-roam-review-refresh)
;;       (:keymaps 'org-mode-map
;;        "C-c r r" '(org-roam-review-accept :wk "accept")
;;        "C-c r u" '(org-roam-review-bury :wk "bury")
;;        "C-c r x" '(org-roam-review-set-excluded :wk "set excluded")
;;        "C-c r b" '(org-roam-review-set-budding :wk "set budding")
;;        "C-c r s" '(org-roam-review-set-seedling :wk "set seedling")
;;        "C-c r e" '(org-roam-review-set-evergreen :wk "set evergreen")))

;;; Code:

(require 'dash)
(require 'org)
(require 'org-drill)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'f)
(require 'ht)
(require 'ts)
(require 'plist)

(defgroup org-roam-review nil
  "Extends org-roam with spaced-repetition review of notes."
  :group 'productivity
  :prefix "org-roam-review-")

(defcustom org-roam-review-cache-file "~/org/.org-roam-review"
  "Location of the cache file for quickly finding review files."
  :group 'org-roam-review
  :type 'file)

(defconst org-roam-review-maturity-values '("seedling" "evergreen" "budding"))

(defcustom org-roam-review-ignored-tags '()
  "A list of tags that define a note should not be imported."
  :group 'org-roam-review
  :type '(list string))

(defcustom org-roam-review-tags-ignored-for-review-buffer '("outline")
  "A list of tags that define a note should not be considered a
candidate for reviews."
  :group 'org-roam-review
  :type '(list string))

(defcustom org-roam-review-pending-todo-keywords '("WAIT" "TODO")
  "Org TODO keywords representing pending todos in outline files."
  :group 'org-roam-review
  :type '(repeat string))

(defface org-roam-review-instructions
  '((t
     (:inherit font-lock-comment-face)))
  "Face for instructional information in a review buffer."
  :group 'org-roam-review)

(defface org-roam-review-filter
  '((t
     (:inherit org-tag)))
  "Face for filter information in a review buffer."
  :group 'org-roam-review)

(defface org-roam-review-filter-keyword
  '((t
     (:inherit org-document-info-keyword)))
  "Face for the filter information keyword in a review buffer."
  :group 'org-roam-review)

(defvar org-roam-review-note-accepted-hook nil)
(defvar org-roam-review-note-buried-hook nil)
(defvar org-roam-review-note-processed-hook nil)


;;; Cached note type & accessors

(plist-define org-roam-review-note
  :required (:id :title :file)
  :optional (:tags :next-review :last-review :maturity :todo-keywords :created))

(plist-define org-roam-review-filter
  :optional (:required :forbidden))


;;; Define cache operations

;; Maintain a cache file to ensure review sessions are as responsive as
;; possible.

;; Define plumbing commands for cache here.

(defvar org-roam-review--cache nil)

(defun org-roam-review--cache ()
  (unless org-roam-review--cache
    (setq org-roam-review--cache
          (or (ignore-errors (ht-from-alist (read (f-read-text org-roam-review-cache-file))))
              (make-hash-table :test #'equal))))
  org-roam-review--cache)

(defun org-roam-review--cache-mutate (fn)
  (let ((cache (org-roam-review--cache)))
    (funcall fn cache)
    (f-write-text (prin1-to-string (ht-to-alist cache)) 'utf-8 org-roam-review-cache-file)
    cache))

(defun org-roam-review--cache-clear ()
  (setq org-roam-review--cache nil)
  (when (file-exists-p org-roam-review-cache-file)
    (delete-file org-roam-review-cache-file)))

;; Define cache-management porcelain in terms of plumbing.

(defun org-roam-review--cache-skip-note-p (file)
  (cl-assert file)
  (org-with-wide-buffer
   (save-match-data
     (or (org-entry-get-with-inheritance "REVIEW_EXCLUDED")
         (org-roam-review--daily-note-p file)
         (seq-intersection org-roam-review-ignored-tags (org-roam-review--file-or-headline-tags))))))

(defun org-roam-review--todo-keywords-in-buffer ()
  (save-excursion
    (save-match-data
      (let ((acc)
            (case-fold-search))
        (goto-char (point-min))
        (while (search-forward-regexp org-todo-regexp nil t)
          (push (match-string-no-properties 1) acc))
        (seq-uniq acc)))))

(defun org-roam-review-notes-from-buffer (buf file)
  (with-current-buffer  buf
    (org-with-wide-buffer
     (save-match-data
       (goto-char (point-min))
       (let ((acc)
             (buffer-title
              (save-excursion
                (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol) nil t)
                (match-string 1))))

         (while (search-forward-regexp (org-re-property "ID") nil t)
           (unless (org-roam-review--cache-skip-note-p file)
             (let* ((id (match-string-no-properties 3))
                    (item (org-roam-review-note-create
                           :id id
                           :file file
                           :todo-keywords (org-roam-review--todo-keywords-in-buffer)
                           :next-review (-some->> (org-entry-get-with-inheritance "NEXT_REVIEW") (ts-parse-org))
                           :last-review (-some->> (org-entry-get-with-inheritance "LAST_REVIEW") (ts-parse-org))
                           :created (-some->> (org-entry-get-with-inheritance "CREATED") (ts-parse-org))
                           :maturity (org-entry-get-with-inheritance "MATURITY")
                           :title (substring-no-properties (or (org-get-heading t t t) buffer-title))
                           :tags (org-roam-review--file-or-headline-tags))))
               (push item acc))))
         (nreverse acc))))))

(defun org-roam-review-excluded-note-ids-from-buffer (buf file)
  (with-current-buffer buf
    (org-with-wide-buffer
     (save-match-data
       (goto-char (point-min))
       (let ((acc))
         (while (search-forward-regexp (org-re-property "ID") nil t)
           (when (org-roam-review--cache-skip-note-p file)
             (let ((id (match-string-no-properties 3)))
               (push id acc))))
         (nreverse acc))))))

(defun org-roam-review--update-by-props-in-buffer (cache buf file)
  (dolist (note (org-roam-review-notes-from-buffer buf file))
    (puthash (org-roam-review-note-id note) note cache))
  (dolist (id (org-roam-review-excluded-note-ids-from-buffer buf file))
    (remhash id cache)))

(defun org-roam-review--daily-note-p (file)
  "Test whether the current buffer is a daily note.

This is a wrapper that makes sure org-roam-directory is well-formed.

See:
https://github.com/org-roam/org-roam/issues/2032"
  (cl-assert (or file (buffer-file-name)))
  (let ((org-roam-directory (string-remove-suffix org-roam-dailies-directory org-roam-directory)))
    (org-roam-dailies--daily-note-p file)))

(defun org-roam-review--cache-update ()
  "Update the evergreen notes cache from `after-save-hook'."
  (when (and (derived-mode-p 'org-mode)
             (not (org-roam-review--daily-note-p (buffer-file-name))))
    (org-roam-review--cache-mutate (lambda (cache)
                                     (org-roam-review--update-by-props-in-buffer cache
                                                                                 (current-buffer)
                                                                                 (buffer-file-name))))))

(defun org-roam-review--cache-collect (fn)
  (let ((table (copy-hash-table (org-roam-review--cache))))
    (maphash (lambda (key note)
               (if-let* ((result (funcall fn note)))
                   (puthash key result table)
                 (remhash key table)))
             table)
    (hash-table-values table)))

(defun org-roam-review--file-or-headline-tags ()
  (seq-map #'substring-no-properties
           (if (org-before-first-heading-p)
               org-file-tags
             (org-get-tags))))

(defun org-roam-review--cache-roam-files ()
  (f-files org-roam-directory
           (lambda (file)
             (when (f-ext-p file "org")
               (with-temp-buffer
                 (let ((org-inhibit-startup t))
                   (insert-file-contents file)
                   (org-mode)
                   (unless (org-roam-review--cache-skip-note-p file)
                     (org-roam-review--cache-mutate (lambda (cache)
                                                      (org-roam-review--update-by-props-in-buffer cache
                                                                                                  (current-buffer)
                                                                                                  file))))))))
           t))

;;;###autoload
(defun org-roam-review-cache-rebuild ()
  "Rebuild the evergreen notes cache."
  (interactive)
  (org-roam-review--cache-clear)
  (org-roam-review--cache-roam-files)
  ;; Write back to disk.
  (org-roam-review--cache-mutate #'ignore)
  (message "Rebuilt evergreen notes index."))

;;;###autoload
(defun org-roam-review-cache-show ()
  "Show the contents the evegreen notes cache for debugging."
  (interactive)
  (pp-display-expression (ht-to-plist (org-roam-review--cache))
                         "*org-roam-review cache*"))

;;;###autoload
(define-minor-mode org-roam-review-cache-mode
  "Minor mode to enable book-keeping used for notes reviews"
  :group 'org-roam-review
  (if org-roam-review-cache-mode
      (add-hook 'after-save-hook #'org-roam-review--cache-update nil t)
    (remove-hook 'after-save-hook #'org-roam-review--cache-update t)))


;;; Review buffers

(defvar org-roam-review--filter nil)

(defun org-roam-review-note-ignored-p (note)
  (let* ((tags (org-roam-review-note-tags note))
         (forbidden-tags (append org-roam-review-ignored-tags
                                 (org-roam-review-filter-forbidden org-roam-review--filter)))
         (required-tags (org-roam-review-filter-required org-roam-review--filter)))
    (or (seq-intersection tags forbidden-tags)
        (seq-difference required-tags tags))))

(defun org-roam-review-note-due-p (note)
  (when-let* ((next-review (org-roam-review-note-next-review note)))
    (ts<= next-review (ts-now))))

(defvar-local org-roam-review-buffer-refresh-command nil)

(defun org-roam-review-buffers ()
  (seq-filter (lambda (buf)
                (and (buffer-live-p buf)
                     (with-current-buffer buf
                       (derived-mode-p 'org-roam-review-mode))))
              (buffer-list)))

(defun org-roam-review-refresh (&optional interactive-p)
  "Rebuild the review buffer.

INTERACTIVE-P indicates that the function was called
interactively. Extra messages will be logged."
  (interactive "P")
  (dolist (buf (org-roam-review-buffers))
    (with-current-buffer buf
      (unless org-roam-review-buffer-refresh-command
        (error "Refresh command not defined"))
      (if (commandp org-roam-review-buffer-refresh-command)
          (call-interactively org-roam-review-buffer-refresh-command)
        (funcall org-roam-review-buffer-refresh-command))))
  (when interactive-p
    (message "Buffer refreshed")))

(defun org-roam-review--read-tags-filter ()
  (-let* ((current-filter
           (string-join (append
                         (seq-map (lambda (it) (concat "-" it)) (org-roam-review-filter-forbidden org-roam-review--filter))
                         (org-roam-review-filter-required org-roam-review--filter))
                        " "))
          (input (read-string "Tags filter (+/-): " current-filter))
          ((forbidden required) (-separate (lambda (it) (string-prefix-p "-" it))
                                           (split-string input " " t))))
    (org-roam-review-filter-create :forbidden (seq-map (lambda (it) (string-remove-prefix "-" it))
                                                       forbidden)
                                   :required (seq-map (lambda (it) (string-remove-prefix "+" it))
                                                      required))))

(defun org-roam-review-modify-tags (tags-filter)
  (interactive (list (org-roam-review--read-tags-filter)))
  (setq org-roam-review--filter tags-filter)
  (org-roam-review-refresh t))

(defvar org-roam-review-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "/") #'org-roam-review-modify-tags)
    (define-key keymap (kbd "TAB") #'magit-section-cycle)
    (define-key keymap (kbd "g") #'org-roam-review-refresh)
    (define-key keymap [remap org-roam-buffer-refresh] #'org-roam-review-refresh)
    keymap))

(defun org-roam-review--refresh-buffer-override (fn &rest args)
  (if (equal (buffer-name) org-roam-buffer)
      (apply fn args)
    (call-interactively 'org-roam-review-refresh)))

(define-derived-mode org-roam-review-mode org-roam-mode "Org-roam-review"
  "Major mode for displaying relevant information about Org-roam
nodes for review."
  :group 'org-roam-review
  ;; HACK: avoid all calls to org-roam-buffer-review if we're in a review
  ;; buffer, since it will error.
  (advice-add 'org-roam-buffer-refresh :around #'org-roam-review--refresh-buffer-override))

(defun org-roam-review-insert-preview (node)
  (let (start content)
    (with-temp-buffer
      (insert-file-contents (org-roam-node-file node))
      (goto-char (point-min))
      (org-roam-end-of-meta-data t)
      (setq start (point))
      (org-next-visible-heading 1)
      (setq content
            (org-roam-fontify-like-in-org-mode
             (string-trim (buffer-substring-no-properties start (point))))))

    (magit-insert-section section (org-roam-preview-section)
      (insert (if (string-blank-p (string-trim-left content))
                  (propertize "(Empty)" 'font-lock-face 'font-lock-comment-face)
                content))
      (oset section file (org-roam-node-file node))
      (oset section point start)
      (insert "\n\n"))))

(defun org-roam-review--insert-node (node skip-preview-p insert-preview-fn)
  (magit-insert-section section (org-roam-node-section nil t)
    (magit-insert-heading (propertize (org-roam-node-title node)
                                      'font-lock-face 'magit-section-secondary-heading))
    (oset section node node)
    (unless skip-preview-p
      (funcall insert-preview-fn node))))

(defvar org-roam-review-default-placeholder
  (propertize "(None)" 'face 'font-lock-comment-face))

(defconst org-roam-review-max-previews-per-group
  50)

(defun org-roam-review--insert-notes (notes placeholder insert-preview-fn)
  (if-let* ((nodes (nreverse (seq-reduce (lambda (acc note)
                                           (if-let* ((node (-some->> note
                                                             (org-roam-review-note-id)
                                                             (org-roam-node-from-id))))
                                               (cons node acc)
                                             acc))
                                         notes nil))))
      (--each-indexed nodes
        (let ((skip-preview-p (> (1+ it-index) org-roam-review-max-previews-per-group)))
          (org-roam-review--insert-node it skip-preview-p insert-preview-fn)))
    (insert (or placeholder org-roam-review-default-placeholder))
    (newline)))

(cl-defun org-roam-review--create-buffer
    (&key title instructions group-on placeholder sort postprocess
          (buffer-name "*org-roam-review*")
          (insert-preview-fn 'org-roam-review-insert-preview)
          (notes nil notes-supplied-p))
  "Create a note review buffer for the notes currently in the cache.


The following keyword arguments are required:

- TITLE is the header line for the buffer.

- INSTRUCTIONS is a paragraph inserted below the title. It is
  automatically paragraph-filled.

- NOTES is a list of notes to display (which is possibly empty),
  or a function returning a list of notes.

The following keyword arguments are optional:

- PLACEHOLDER is a string to be shown if there are no notes to
  display.

- POSTPROCESS is a function called after the buffer has been
  populated.

- BUFFER-NAME is the name to use for the created buffer.

- INSERT-PREVIEW-FN is a function that takes a node and is
  expected to insert a preview using the magit-section API.

- GROUP-ON is a projection function that is passed a note and
  should return one of:

    - nil, meaning the note should be omitted

    - a string to use for grouping the note

    - a cons of `(GROUP-NAME . GROUP-PRIORITY)', where:

        - GROUP-NAME is the string for grouping the note

        - GROUP-PRIORITY is a number used to order group in the
          buffer.

- SORT is a projection function that is passed two notes within a
  group and returns non-nil if the first element should sort
  before the second."
  (cl-assert (and notes-supplied-p title))
  (let ((buf (get-buffer-create buffer-name)))
    (cl-labels ((render-buffer
                 ()
                 (let ((notes (if (functionp notes) (funcall notes) notes)))
                   (with-current-buffer buf
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (org-roam-review-mode)
                       (org-roam-buffer-set-header-line-format title)
                       (setq-local org-roam-review-buffer-refresh-command (lambda () (render-buffer)))
                       (magit-insert-section (root)
                         (when (and instructions notes)
                           (let ((start (point)))
                             (insert (propertize instructions 'font-lock-face 'org-roam-review-instructions))
                             (fill-region start (point)))
                           (newline 2))

                         (let ((forbidden-tags (seq-map (lambda (it) (format "-%s" it)) (org-roam-review-filter-forbidden org-roam-review--filter)))
                               (required-tags (seq-map (lambda (it) (format "+%s" it)) (org-roam-review-filter-required org-roam-review--filter))))
                           (when (or forbidden-tags required-tags)
                             (insert (concat (propertize "Filters:" 'face 'org-roam-review-filter-keyword)
                                             " "
                                             (propertize (string-join (append forbidden-tags required-tags) " ") 'face 'org-roam-review-filter)))
                             (newline 2)))

                         (cond ((null notes)
                                (insert (or placeholder org-roam-review-default-placeholder))
                                (newline))
                               (group-on
                                (let ((grouped (->> (seq-group-by group-on notes)
                                                    (-sort (-on #'<= (-lambda ((key . _))
                                                                       (if (stringp key) key (or (cdr key) 0))))))))
                                  (pcase-dolist (`(,key . ,group) grouped)
                                    (when (and key group)
                                      (magit-insert-section (org-roam-review-note-group)
                                        (let ((header (format "%s (%s)"
                                                              (if (stringp key) key (car key))
                                                              (length group))))
                                          (magit-insert-heading (propertize header 'font-lock-face 'magit-section-heading)))
                                        (org-roam-review--insert-notes (-sort (or sort (-const t)) group) placeholder insert-preview-fn)
                                        (insert "\n"))))))
                               (t
                                (org-roam-review--insert-notes (-sort (or sort (-const t)) notes) placeholder insert-preview-fn))))
                       (goto-char (point-min))
                       (save-excursion
                         (when postprocess (funcall postprocess)))
                       buf)))))
      (render-buffer))))

;;;###autoload
(defun org-roam-review (&optional all)
  "List notes that are due for review.

With optional prefix arg ALL, list all evergreen notes
categorised by their maturity."
  (interactive "P")
  (if all
      (org-roam-review-list-categorised)
    (org-roam-review-list-due)))

(defun org-roam-review--maturity-header-for-note (note)
  (pcase (org-roam-review-note-maturity note)
    ("seedling" (cons "Seedling 🌱" 3))
    ("budding" (cons "Budding 🪴" 2))
    ("evergreen" (cons "Evergreen 🌲" 1))
    (value value)))

(defun org-roam-review-display-buffer-and-select (buf)
  (display-buffer buf)
  (when-let* ((win (seq-find (lambda (it) (equal buf (window-buffer it)))
                             (window-list))))
    (select-window win)))

;;;###autoload
(defun org-roam-review-list-due ()
  "List notes that are due for review."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review--create-buffer
    :title "Due Notes"
    :instructions "The notes below are due for review.
Read each note and add new thoughts and connections, then mark
them as reviewed with `org-roam-review-accept',
`org-roam-review-bury' or by updating their maturity."
    :placeholder (concat (propertize "You're up-to-date!" 'face 'font-lock-comment-face) " 😸")
    :group-on #'org-roam-review--maturity-header-for-note
    :sort (-on #'ts< #'org-roam-review-note-next-review)
    :notes
    (lambda ()
      (org-roam-review--cache-collect
       (lambda (note)
         (when (and (not (org-roam-review-note-ignored-p note))
                    (null (seq-intersection (org-roam-review-note-tags note)
                                            org-roam-review-tags-ignored-for-review-buffer))
                    (org-roam-review-note-due-p note))
           note)))))))

;;;###autoload
(defun org-roam-review-list-categorised ()
  "List all evergreen notes categorised by maturity."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review--create-buffer
    :title "Evergreen Notes"
    :instructions "The notes below are categorised by maturity."
    :group-on #'org-roam-review--maturity-header-for-note
    :sort (-on #'string-lessp #'org-roam-review-note-title)
    :notes
    (lambda ()
      (org-roam-review--cache-collect
       (lambda (note)
         (when (and (not (org-roam-review-note-ignored-p note))
                    (org-roam-review-note-maturity note))
           note)))))))

;;;###autoload
(defun org-roam-review-list-uncategorised ()
  "List notes missing required properties to be used for reviews.

This is useful for migrating notes into the spaced repetition
system."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review--create-buffer
    :title "Uncategorised Notes"
    :instructions "The notes below are missing the properties
needed to be included in reviews. Categorise them as appropriate."
    :sort (-on #'string-lessp #'org-roam-review-note-title)
    :notes
    (lambda ()
      (org-roam-review--cache-collect
       (lambda (note)
         (unless (or (org-roam-review-note-ignored-p note)
                     (seq-contains-p (org-roam-review-note-tags note) "outline")
                     (seq-intersection (org-roam-review-note-tags note)
                                       org-roam-review-tags-ignored-for-review-buffer)
                     (org-roam-review-note-maturity note)
                     (org-roam-review-note-next-review note))
           note)))))))

;;;###autoload
(defun org-roam-review-list-authors ()
  "List all author notes."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review--create-buffer
    :title "Author Notes"
    :instructions "The list below contains notes tagged as authors."
    :sort (-on #'string-lessp #'org-roam-review-note-title)
    :notes
    (lambda ()
      (org-roam-review--cache-collect
       (lambda (note)
         (when (seq-contains-p (org-roam-review-note-tags note) "author")
           note)))))))

(defun org-roam-review--note-todo-presence (note)
  (if (seq-intersection (org-roam-review-note-todo-keywords note)
                        org-roam-review-pending-todo-keywords)
      (cons "Unfinished" 1)
    (cons "Complete" 2)))

;;;###autoload
(defun org-roam-review-list-outlines ()
  "List all outline notes."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review--create-buffer
    :title "Outline Notes"
    :instructions "The notes below are outlines of sources,
grouped by whether they require further processing."
    :group-on #'org-roam-review--note-todo-presence
    :sort (-on #'string-lessp #'org-roam-review-note-title)
    :notes (org-roam-review--cache-collect
            (lambda (note)
              (when (seq-contains-p (org-roam-review-note-tags note) "outline")
                note))))))

(defun org-roam-review--note-added-group (note)
  (when-let* ((created (org-roam-review-note-created note))
              (recently (ts-adjust 'hour -24 (ts-now))))
    (cond
     ((ts<= recently created)
      (cons "Recent" 1))
     ((ts<= (ts-adjust 'day -3 recently) created)
      (cons "Last 3 days" 2))
     ((ts<= (ts-adjust 'day -7 recently) created)
      (cons "Last week" 3)))))

;;;###autoload
(defun org-roam-review-list-recently-added ()
  "List notes that were created recently, grouped by time."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review--create-buffer
    :title "Recently Created Notes"
    :instructions "The notes below are sorted by when they were created."
    :group-on #'org-roam-review--note-added-group
    :sort (-on #'string-lessp #'org-roam-review-note-title)
    :notes
    (lambda ()
      (org-roam-review--cache-collect
       (lambda (note)
         (unless (seq-intersection (org-roam-review-note-tags note) org-roam-review-ignored-tags)
           note)))))))



(defun org-roam-review--update-next-review (quality)
  "Adapted from org-drill.

- only use sm5 algorithm for simplicity
- use properties instead of SCHEDULED.
- remove support for 'weighting' a note."
  (-let* ((ofmatrix org-drill-sm5-optimal-factor-matrix)
          ((last-interval repetitions failures total-repeats meanq ease) (org-drill-get-item-data))
          ((next-interval repetitions ease failures meanq total-repeats new-ofmatrix)
           (org-drill-determine-next-interval-sm5 last-interval repetitions
                                                  ease quality failures
                                                  meanq total-repeats ofmatrix))
          (next-interval (round (if (cl-minusp next-interval)
                                    next-interval
                                  (max 1.0 (+ last-interval (- next-interval last-interval))))))
          (new-time (ts-adjust 'day next-interval (ts-now))))
    (setq org-drill-sm5-optimal-factor-matrix new-ofmatrix)
    (org-drill-store-item-data next-interval repetitions failures total-repeats meanq ease)

    (let ((next-review (ts-format "[%Y-%m-%d %a]" new-time)))
      (org-set-property "NEXT_REVIEW" next-review)
      next-review)))

(defun org-roam-review--update-note (maturity score)
  "Set the MATURITY and updated SCORE for a note.

A higher score means that the note will appear less frequently."
  (cl-assert (member maturity org-roam-review-maturity-values))
  (cl-assert (derived-mode-p 'org-mode))
  (when (org-roam-review--daily-note-p (buffer-file-name))
    (user-error "Cannot set maturity on daily file"))
  (let ((id (org-entry-get-with-inheritance "ID" t)))
    (unless id
      (error "No ID property for tree at point"))
    (org-with-point-at (org-find-property "ID" id)
      (atomic-change-group
        (let ((next-review (org-roam-review--update-next-review score)))
          (ignore-errors
            (org-roam-tag-remove org-roam-review-maturity-values))
          (org-roam-tag-add (list maturity))

          (org-delete-property "REVIEW_EXCLUDED")
          (org-set-property "MATURITY" maturity)
          (org-set-property "LAST_REVIEW" (org-format-time-string "[%Y-%m-%d %a]"))

          (save-buffer)
          (message "Maturity set to '%s'. Review scheduled for %s" maturity next-review)))))
  (org-roam-review-refresh))

(defun org-roam-review--kill-buffer-for-completed-review ()
  (let ((review-buf (get-buffer "*org-roam-review*")))
    (mapc (lambda (win)
            (when (equal review-buf (window-buffer win))
              (delete-window win)))
          (window-list))
    (save-buffer)
    (kill-buffer)
    (-some->> review-buf
      (org-roam-review-display-buffer-and-select)
      (select-window))))

;;;###autoload
(defun org-roam-review-accept ()
  "Confirm review of the current note."
  (interactive)
  (when-let* ((maturity (org-entry-get-with-inheritance "MATURITY")))
    (org-roam-review--update-note maturity 3))
  (org-roam-review--kill-buffer-for-completed-review)
  (run-hooks 'org-roam-review-note-accepted-hook)
  (run-hooks 'org-roam-review-note-processed-hook)
  (org-roam-review-refresh))

;;;###autoload
(defun org-roam-review-bury ()
  "Confirm review of the current note and bury it."
  (interactive)
  (when-let* ((maturity (org-entry-get-with-inheritance "MATURITY")))
    (org-roam-review--update-note maturity 5))
  (org-roam-review--kill-buffer-for-completed-review)
  (run-hooks 'org-roam-review-note-buried-hook)
  (run-hooks 'org-roam-review-note-processed-hook)
  (org-roam-review-refresh))

(defun org-roam-review--skip-note-for-maturity-assignment-p ()
  (org-with-wide-buffer
   (or (org-roam-review--daily-note-p (buffer-file-name))
       (seq-intersection org-roam-review-ignored-tags (org-roam-review--file-or-headline-tags)))))

;;;###autoload
(defun org-roam-review-set-budding (&optional bury)
  "Set the current note as a 'budding' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (unless (org-roam-review--skip-note-for-maturity-assignment-p)
    (org-roam-review--update-note "budding" (if bury 5 3))))

;;;###autoload
(defun org-roam-review-set-seedling (&optional bury)
  "Set the current note as a 'seedling' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (unless (org-roam-review--skip-note-for-maturity-assignment-p)
    (org-roam-review--update-note "seedling" (if bury 5 1))))

;;;###autoload
(defun org-roam-review-set-evergreen (&optional bury)
  "Set the current note as an 'evergreen' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (unless (org-roam-review--skip-note-for-maturity-assignment-p)
    (org-roam-review--update-note "evergreen" (if bury 5 4))))

(defconst org-roam-review--properties
  '("LAST_REVIEW"
    "NEXT_REVIEW"
    "MATURITY"
    "DRILL_LAST_INTERVAL"
    "DRILL_REPEATS_SINCE_FAIL"
    "DRILL_TOTAL_REPEATS"
    "DRILL_FAILURE_COUNT"
    "DRILL_AVERAGE_QUALITY"
    "DRILL_EASE")
  "List of properties managed by `org-roam-review'.")

(defun org-roam-review-remove-managed-properties-in-node (node-id)
  (let ((message-log-max))
    (org-with-point-at (org-find-property "ID" node-id)
      (ignore-errors
        (org-roam-tag-remove org-roam-review-maturity-values))
      (dolist (name org-roam-review--properties)
        (org-delete-property name)))))

;;;###autoload
(defun org-roam-review-set-excluded ()
  "Exclude this note from reviews.

This is useful for notes that are not Evergreens, e.g. wiki-style
notes that aren't expected to be refined over time.

This sets a special property, REVIEW_EXCLUDED, to indicate that
it is not a candidate for reviews."
  (interactive)
  (let ((id (org-entry-get-with-inheritance "ID" t)))
    (unless id
      (error "No ID property for tree at point"))
    (org-with-point-at (org-find-property "ID" id)
      (atomic-change-group
        (org-roam-review-remove-managed-properties-in-node id)
        (org-set-property "REVIEW_EXCLUDED" "t"))
      (save-buffer))

    (let ((title (org-roam-node-title (org-roam-node-from-id id))))
      (message "Excluded note `%s' from reviews" title))))

;;;###autoload
(defun org-roam-review-set-author ()
  "Mark this note as an author note."
  (interactive)
  (atomic-change-group
    (org-with-wide-buffer
     (let ((id (org-entry-get-with-inheritance "ID" t)))
       (unless id
         (error "No ID property for tree at point"))
       (org-roam-review-remove-managed-properties-in-node id)
       (org-roam-tag-add '("author"))
       (save-buffer)))))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
