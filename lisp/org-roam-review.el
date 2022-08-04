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
;;       (org-mode . org-roam-note-cache-mode)
;;       (org-roam-capture-new-node . org-roam-review-set-seedling)
;;       :custom
;;       (org-roam-note-ignored-tags '("person" "client" "project" "lit_notes"))
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

(require 'org-roam-note)
(require 'org-drill)

(defgroup org-roam-review nil
  "Extends org-roam with spaced-repetition review of notes."
  :group 'productivity
  :prefix "org-roam-review-")

(defconst org-roam-review-maturity-values '("seedling" "evergreen" "budding"))

(defconst org-roam-review-maturity-emoji-alist
  '(("seedling" . "🌱")
    ("budding" . "🪴")
    ("evergreen" . "🌲")))

(defcustom org-roam-review-tags-ignored-for-review-buffer '("outline")
  "A list of tags that disqualify a note from review."
  :group 'org-roam-review
  :type '(list string))

(defcustom org-roam-review-pending-todo-keywords '("WAIT" "TODO")
  "Org TODO keywords representing pending todos in outline files."
  :group 'org-roam-review
  :type '(repeat string))

(defcustom org-roam-review-show-instructions-p t
  "Whether to show instructions in review buffers."
  :group 'org-roam-review
  :type 'boolean)

(defface org-roam-review-instructions
  '((t
     (:inherit font-lock-comment-face)))
  "Face for instructional information in a review buffer."
  :group 'org-roam-review)

(defface org-roam-review-heading
  '((t (org-level-2 :bold t)))
  "Face for headings in review buffers."
  :group 'org-roam-review)

(defface org-roam-review-heading-separator
  '((t
     (:inherit org-level-2)))
  "Face for heading separators in review buffers."
  :group 'org-roam-review)

(defvar org-roam-note-accepted-hook nil)
(defvar org-roam-note-buried-hook nil)
(defvar org-roam-note-processed-hook nil)


;;; Review buffers

(defun org-roam-note-due-p (note)
  (when-let* ((next-review (org-roam-note-next-review note)))
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
      (funcall org-roam-review-buffer-refresh-command)))
  (when interactive-p
    (message "Buffer refreshed")))

(defun org-roam-review--pp-tags-filter (tags-filter)
  (string-join (append
                (seq-map (lambda (it) (concat "-" it)) (org-roam-note-filter-forbidden tags-filter))
                (org-roam-note-filter-required tags-filter)) " "))

(defun org-roam-review--read-tags-filter (&optional prompt)
  (let* ((current-filter (org-roam-review--pp-tags-filter org-roam-note-last-filter))
         (input (read-string (or prompt "Tags filter (+/-): ")
                             (unless  (string-blank-p current-filter)
                               (concat current-filter " "))
                             'org-roam-review-tags)))
    (org-roam-note-filter-parse input)))

(defun org-roam-review-modify-tags (tags-filter &optional no-refresh)
  "Read tags filter interactively.

TAGS-FILTER is plist of type `org-roam-note-filter'.

NO-REFRESH means don't update open org-roam-review buffers.

When called with a `C-u' prefix arg, clear the current filter."
  (interactive (list
                (unless current-prefix-arg
                  (org-roam-review--read-tags-filter))))
  (setq org-roam-note-last-filter tags-filter)
  (unless no-refresh
    (org-roam-review-refresh t)))

(defvar org-roam-review-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "/") #'org-roam-review-modify-tags)
    (define-key keymap (kbd "TAB") #'magit-section-cycle)
    (define-key keymap (kbd "g") #'org-roam-review-refresh)
    (define-key keymap (kbd "C-c r r") #'org-roam-review-accept)
    (define-key keymap (kbd "C-c r u") #'org-roam-review-bury)
    (define-key keymap (kbd "C-c r x") #'org-roam-review-set-excluded)
    (define-key keymap [remap org-roam-buffer-refresh] #'org-roam-review-refresh)
    keymap))

(defun org-roam-review--refresh-buffer-override (fn &rest args)
  (if (equal (buffer-name) org-roam-buffer)
      (apply fn args)
    (call-interactively 'org-roam-review-refresh)))

(define-derived-mode org-roam-review-mode org-roam-mode "Org-roam-review"
  "Major mode for displaying relevant information about Org-roam nodes for review."
  :group 'org-roam-review
  ;; HACK: avoid all calls to org-roam-buffer-review if we're in a review
  ;; buffer, since it will error.
  (advice-add 'org-roam-buffer-refresh :around #'org-roam-review--refresh-buffer-override))

(defvar org-roam-review-indent-width 2)

(defun org-roam-review-indent-string (str depth)
  (replace-regexp-in-string (rx bol) (make-string (* depth org-roam-review-indent-width) 32)
                            str))

(cl-defun org-roam-review-insert-preview (node &optional hidden-p (depth 0))
  (let* ((start (org-roam-node-point node))
         (content (org-roam-fontify-like-in-org-mode (org-roam-preview-get-contents (org-roam-node-file node) start))))
    (magit-insert-section section (org-roam-preview-section nil hidden-p)
      (insert (org-roam-review-indent-string (if (string-blank-p (string-trim-left content))
                                                 (propertize "(Empty)" 'font-lock-face 'font-lock-comment-face)
                                               content)
                                             depth))
      (oset section file (org-roam-node-file node))
      (oset section point start)
      (insert "\n\n"))))

(defun org-roam-review--insert-node (node insert-preview-fn)
  (catch 'skip
    (atomic-change-group
      (magit-insert-section section (org-roam-node-section nil t)
        (magit-insert-heading (propertize (org-roam-node-title node)
                                          'font-lock-face 'magit-section-secondary-heading))
        (oset section node node)
        ;; FIXME: expansion breaks visiting node from the heading above.
        (magit-insert-section-body
          (funcall insert-preview-fn node))))))

(defvar org-roam-review-default-placeholder
  (propertize "(None)" 'face 'font-lock-comment-face))

(defconst org-roam-review-max-previews-per-group
  50)

(defun org-roam-review--insert-notes (notes placeholder insert-preview-fn)
  (if-let* ((nodes (nreverse (seq-reduce (lambda (acc note)
                                           (if-let* ((node (-some->> note
                                                             (org-roam-note-id)
                                                             (org-roam-node-from-id))))
                                               (cons node acc)
                                             acc))
                                         notes nil))))
      (--each nodes
        (org-roam-review--insert-node it insert-preview-fn))
    (insert (or placeholder org-roam-review-default-placeholder))
    (newline)))

(plist-define org-roam-review-render-args
  :optional (:group-on :notes :placeholder :sort)
  :required (:insert-preview-fn :root))

(cl-defun org-roam-review--insert-notes-fn-default (args)
  (-let* (((&plist :group-on :notes :placeholder :sort :insert-preview-fn :root) args)
          (sort (or sort (-const t))))
    (cond
     ((null notes)
      (insert (or placeholder org-roam-review-default-placeholder))
      (newline))
     (group-on
      (let ((grouped (->> (seq-group-by group-on notes)
                          (-sort (-on #'<= (-lambda ((key . _))
                                             (if (stringp key) key (or (cdr key) 0))))))))
        (pcase-dolist (`(,key . ,group) grouped)
          (when (and key group)
            (magit-insert-section section (org-roam-note-group)
              (oset section parent root)
              (let ((header (format "%s (%s)"
                                    (if (stringp key) key (car key))
                                    (length group))))
                (magit-insert-heading (propertize header 'font-lock-face 'magit-section-heading)))
              (org-roam-review--insert-notes (-sort sort group) placeholder insert-preview-fn)
              (insert "\n"))))))
     (t
      (org-roam-review--insert-notes (-sort sort notes) placeholder insert-preview-fn)))))

(cl-defun org-roam-review--render (&key insert-notes-fn title instructions group-on placeholder sort insert-preview-fn notes)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (org-roam-review-mode)
    (org-roam-buffer-set-header-line-format title)
    (magit-insert-section root (root)
      (when (and org-roam-review-show-instructions-p instructions notes)
        (let ((start (point)))
          (insert (propertize instructions 'font-lock-face 'org-roam-review-instructions))
          (fill-region start (point)))
        (newline 2))

      (let ((forbidden-tags (seq-map (lambda (it) (format "-%s" it)) (org-roam-note-filter-forbidden org-roam-note-last-filter)))
            (required-tags (seq-map (lambda (it) (format "+%s" it)) (org-roam-note-filter-required org-roam-note-last-filter))))
        (when (or forbidden-tags required-tags)
          (insert (concat (propertize "Filters:" 'face 'org-roam-note-filter-keyword)
                          " "
                          (propertize (string-join (append forbidden-tags required-tags) " ") 'face 'org-roam-note-filter)))
          (newline 2)))

      (let ((start-of-content (point)))
        (funcall insert-notes-fn
                 (org-roam-review-render-args-create :notes notes
                                                     :group-on group-on
                                                     :sort sort
                                                     :root root
                                                     :placeholder placeholder
                                                     :insert-preview-fn insert-preview-fn))
        (goto-char start-of-content)))))

(cl-defun org-roam-review-create-buffer
    (&key title instructions group-on placeholder sort notes
          (buffer-name "*org-roam-review*")
          (insert-notes-fn 'org-roam-review--insert-notes-fn-default)
          (insert-preview-fn 'org-roam-review-insert-preview))
  "Create a note review buffer for the notes currently in the cache.


The following keyword arguments are required:

- TITLE is the header line for the buffer.

- INSTRUCTIONS is a paragraph inserted below the title. It is
  automatically paragraph-filled.

- NOTES is a function returning a list of notes to display (which
  is possibly empty).

The following keyword arguments are optional:

- PLACEHOLDER is a string to be shown if there are no notes to
  display.

- BUFFER-NAME is the name to use for the created buffer.

- INSERT-NOTES-FN is a function taking a plist of type
  `org-roam-review-render-args' to override the default render
  behaviour for notes. It is expected to insert a rendered
  representation of notes using the magit-section API.

- INSERT-PREVIEW-FN is a function that takes a node and is
  expected to insert a preview using the magit-section API. As a
  special case, throwing an error with a `skip' tag will cause
  insertion of this entry to be skipped. The default
  implementation will show the content before the first heading.

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
  (cl-assert title)
  (cl-assert (functionp notes))
  (let (render)
    (setq render
          (lambda (updated-notes)
            (with-current-buffer (get-buffer-create buffer-name)
              (org-roam-review--render :title title
                                       :instructions instructions
                                       :notes updated-notes
                                       :group-on group-on
                                       :placeholder placeholder
                                       :sort sort
                                       :insert-notes-fn insert-notes-fn
                                       :insert-preview-fn insert-preview-fn)
              (setq-local org-roam-review-buffer-refresh-command (lambda () (funcall render (funcall notes))))
              (current-buffer))))
    (funcall render (funcall notes))))

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
  (pcase (org-roam-note-maturity note)
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
   (org-roam-review-create-buffer
    :title "Due Notes"
    :instructions "The notes below are due for review.
Read each note and add new thoughts and connections, then mark
them as reviewed with `org-roam-review-accept',
`org-roam-review-bury' or by updating their maturity."
    :placeholder (concat (propertize "You're up-to-date!" 'face 'font-lock-comment-face) " 😸")
    :group-on #'org-roam-review--maturity-header-for-note
    :sort (-on #'ts< #'org-roam-note-next-review)
    :notes
    (lambda ()
      (org-roam-note-cache-collect
       (lambda (note)
         (when (and (not (org-roam-note-ignored-p note))
                    (zerop (org-roam-note-level note))
                    (null (seq-intersection (org-roam-note-tags note)
                                            org-roam-review-tags-ignored-for-review-buffer))
                    (org-roam-note-due-p note))
           note)))))))

(defalias 'org-roam-review-sort-by-title-case-insensitive
  (-on #'string-lessp (-compose  #'downcase #'org-roam-note-title)))

;;;###autoload
(defun org-roam-review-list-categorised ()
  "List all evergreen notes categorised by maturity."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review-create-buffer
    :title "Evergreen Notes"
    :instructions "The notes below are categorised by maturity."
    :group-on #'org-roam-review--maturity-header-for-note
    :sort #'org-roam-review-sort-by-title-case-insensitive
    :notes
    (lambda ()
      (org-roam-note-cache-collect
       (lambda (note)
         (when (and (not (org-roam-note-ignored-p note))
                    (org-roam-note-maturity note))
           note)))))))

;;;###autoload
(defun org-roam-review-list-uncategorised ()
  "List notes missing required properties to be used for reviews.

This is useful for migrating notes into the spaced repetition
system."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review-create-buffer
    :title "Uncategorised Notes"
    :instructions "The notes below are missing the properties
needed to be included in reviews. Categorise them as appropriate."
    :sort #'org-roam-review-sort-by-title-case-insensitive
    :notes
    (lambda ()
      (org-roam-note-cache-collect
       (lambda (note)
         (unless (or (org-roam-note-ignored-p note)
                     (seq-contains-p (org-roam-note-local-tags note) "outline")
                     (seq-intersection (org-roam-note-tags note)
                                       org-roam-review-tags-ignored-for-review-buffer)
                     (org-roam-note-maturity note)
                     (org-roam-note-next-review note))
           note)))))))

;;;###autoload
(defun org-roam-review-list-authors ()
  "List all author notes."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review-create-buffer
    :title "Author Notes"
    :instructions "The list below contains notes tagged as authors."
    :sort #'org-roam-review-sort-by-title-case-insensitive
    :notes
    (lambda ()
      (org-roam-note-cache-collect
       (lambda (note)
         (when (and (seq-contains-p (org-roam-note-tags note) "author")
                    (not (org-roam-note-ignored-p note)))
           note)))))))

(defun org-roam-review--note-todo-presence (note)
  (if (seq-intersection (org-roam-note-todo-keywords note)
                        org-roam-review-pending-todo-keywords)
      (cons "Unfinished" 1)
    (cons "Complete" 2)))

;;;###autoload
(defun org-roam-review-list-outlines ()
  "List all outline notes."
  (interactive)
  (org-roam-review-display-buffer-and-select
   (org-roam-review-create-buffer
    :title "Outline Notes"
    :instructions "The notes below are outlines of sources,
grouped by whether they require further processing."
    :group-on #'org-roam-review--note-todo-presence
    :sort #'org-roam-review-sort-by-title-case-insensitive
    :notes
    (lambda ()
      (org-roam-note-cache-collect
       (lambda (note)
         (when (and (seq-contains-p (org-roam-note-local-tags note) "outline")
                    (not (org-roam-note-ignored-p note)))
           note)))))))

;;;###autoload
(defun org-roam-review-visit-outline (&optional arg)
  "Choose an ouline note to open.

With a single prefix ARG, show in other another window.

With two prefix args, show the list of outlines instead."
  (interactive "p")
  (if (equal 16 arg)
      (org-roam-review-list-outlines)
    (let* ((notes
            (ht-from-alist
             (org-roam-note-cache-collect
              (lambda (note)
                (when (seq-contains-p (org-roam-note-local-tags note) "outline")
                  (cons (string-remove-prefix "Outline - " (org-roam-note-title note)) note))))))
           (choice (completing-read "Outline: " (ht-keys notes) nil t)))
      (org-roam-node-visit (org-roam-note-to-node (gethash choice notes)) (equal arg 4)))))

(defun org-roam-review--note-added-group (note)
  (when-let* ((created (org-roam-note-created note))
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
   (org-roam-review-create-buffer
    :title "Recently Created Notes"
    :instructions "The notes below are sorted by when they were created."
    :group-on #'org-roam-review--note-added-group
    :sort #'org-roam-review-sort-by-title-case-insensitive
    :notes
    (lambda ()
      (org-roam-note-cache-collect
       (lambda (note)
         (unless (org-roam-note-ignored-p note)
           note)))))))



(defvar org-roam-review--maturity-score-revisit 1)
(defvar org-roam-review--maturity-score-ok 3)
(defvar org-roam-review--maturity-score-bury 5)

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
  (when (org-roam-note--daily-note-p (buffer-file-name))
    (user-error "Cannot set maturity on daily file"))
  (let ((id (org-entry-get (point-min) "ID")))
    (unless id
      (error "Not visiting an Evergreen Note--no ID property found"))
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
          (message "Maturity set to '%s'. Review scheduled for %s" maturity next-review))))))

(defun org-roam-review--update-workspace-for-completed-review ()
  (let ((buf (current-buffer))
        (initial-window-count (length (window-list))))
    (save-buffer)
    (when (< 1 initial-window-count)
      (delete-window))
    (kill-buffer buf)

    (when (= 1 initial-window-count)
      (-some->> (get-buffer "*org-roam-review*")
        (org-roam-review-display-buffer-and-select)
        (select-window)))))

(defmacro org-roam-review--visiting-note-at-point (&rest body)
  (declare (indent 0))
  `(let* ((node (org-roam-node-at-point t))
          (file (org-roam-node-file node)))
     (cond
      (file
       (with-current-buffer (find-file-noselect file)
         (save-excursion
           (goto-char (org-roam-node-point node))
           ,@body)))
      ((derived-mode-p 'org-mode)
       (org-with-wide-buffer
        (point-min)
        ,@body))
      (t
       (error "Invalid context for visiting node")))))

;;;###autoload
(defun org-roam-review-accept ()
  "Confirm review of the current note."
  (interactive)
  (org-roam-review--visiting-note-at-point
    (when-let* ((maturity (org-entry-get-with-inheritance "MATURITY")))
      (org-roam-review--update-note maturity org-roam-review--maturity-score-ok))
    (org-roam-review--update-workspace-for-completed-review)
    (run-hooks 'org-roam-note-accepted-hook)
    (run-hooks 'org-roam-note-processed-hook)
    (message "Note scheduled for future review")))

;;;###autoload
(defun org-roam-review-bury ()
  "Confirm review of the current note and bury it."
  (interactive)
  (org-roam-review--visiting-note-at-point
    (when-let* ((maturity (org-entry-get-with-inheritance "MATURITY")))
      (org-roam-review--update-note maturity org-roam-review--maturity-score-bury))
    (org-roam-review--update-workspace-for-completed-review)
    (run-hooks 'org-roam-note-buried-hook)
    (run-hooks 'org-roam-note-processed-hook)
    (message "Note buried")))

(defun org-roam-review--skip-note-for-maturity-assignment-p ()
  (org-with-wide-buffer
   (or (org-roam-note--daily-note-p (buffer-file-name))
       (seq-intersection org-roam-note-ignored-tags (org-roam-note-file-or-headline-tags)))))

;;;###autoload
(defun org-roam-review-set-budding (&optional bury)
  "Set the current note as a 'budding' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (let ((score (if bury
                   org-roam-review--maturity-score-bury
                 org-roam-review--maturity-score-ok)))
    (org-roam-review--visiting-note-at-point
      (unless (org-roam-review--skip-note-for-maturity-assignment-p)
        (org-roam-review--update-note "budding" score)))))

;;;###autoload
(defun org-roam-review-set-seedling (&optional bury)
  "Set the current note as a 'seedling' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (let ((score (if bury
                   org-roam-review--maturity-score-bury
                 org-roam-review--maturity-score-revisit)))
    (org-roam-review--visiting-note-at-point
      (unless (org-roam-review--skip-note-for-maturity-assignment-p)
        (org-roam-review--update-note "seedling" score)))))

;;;###autoload
(defun org-roam-review-set-evergreen (&optional bury)
  "Set the current note as an 'evergreen' note and confirm it's been reviewed.

With prefix arg BURY, the note is less likely to be surfaced in
the future."
  (interactive "P")
  (let ((score (if bury
                   org-roam-review--maturity-score-bury
                 org-roam-review--maturity-score-ok)))
    (org-roam-review--visiting-note-at-point
      (unless (org-roam-review--skip-note-for-maturity-assignment-p)
        (org-roam-review--update-note "evergreen" score)))))

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
  (org-roam-review--visiting-note-at-point
    (let ((id (org-entry-get (point-min) "ID")))
      (unless id
        (error "No ID in buffer"))
      (org-with-point-at (org-find-property "ID" id)
        (atomic-change-group
          (org-roam-review-remove-managed-properties-in-node id)
          (org-set-property "REVIEW_EXCLUDED" "t"))
        (save-buffer))

      (let ((title (org-roam-node-title (org-roam-node-from-id id))))
        (message "Excluded note `%s' from reviews" title)))))

;;;###autoload
(defun org-roam-review-set-author ()
  "Mark this note as an author note."
  (interactive)
  (org-roam-review--visiting-note-at-point
    (atomic-change-group
      (org-with-wide-buffer
       (let ((id (org-entry-get (point-min) "ID")))
         (unless id
           (error "No ID in buffer"))
         (org-roam-review-remove-managed-properties-in-node id)
         (org-roam-tag-add '("author"))
         (save-buffer))))))

(provide 'org-roam-review)

;;; org-roam-review.el ends here
