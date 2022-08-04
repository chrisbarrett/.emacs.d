;;; org-roam-note.el --- Shared core type for custom org-roam Evergeen Notes layer  -*- lexical-binding: t; -*-

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

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'dash)
(require 'org)
(require 'org-roam)
(require 'org-roam-dailies)
(require 'f)
(require 'ht)
(require 'ts)
(require 'pp)
(require 'plist)

(defgroup org-roam-note nil
  "Extends org-roam with spaced-repetition review of notes."
  :group 'productivity
  :prefix "org-roam-note-")

(defcustom org-roam-note-cache-file "~/org/.org-roam-note"
  "Location of the cache file for quickly finding review files."
  :group 'org-roam-note
  :type 'file)

(defcustom org-roam-note-ignored-tags '()
  "A list of tags that define a note should not be imported."
  :group 'org-roam-note
  :type '(list string))

(defface org-roam-note-filter
  '((t
     (:inherit org-tag)))
  "Face for filter information in a review buffer."
  :group 'org-roam-note)

(defface org-roam-note-filter-keyword
  '((t
     (:inherit org-document-info-keyword)))
  "Face for the filter information keyword in a review buffer."
  :group 'org-roam-note)


;; Org-mode commands for parsing and traversing the buffer require a functioning
;; org-mode, but setting up org-mode for interactive use can be a big
;; performance bottleneck, depending on what's in the mode hooks. Define a way
;; to start a more minimal org-mode.

(defmacro org-roam-note--with-ignored-functions (function-names &rest body)
  (declare (indent 1))
  `(progn
     ,@(seq-map (lambda (it) `(advice-add #',it :override #'ignore))
                function-names)
     (unwind-protect
         (progn
           ,@body)
       ,@(seq-map (lambda (it)
                    `(advice-remove #',it #'ignore))
                  function-names))))

(defun org-roam-note--hacky-org-mode-enable ()
  (org-roam-note--with-ignored-functions (run-mode-hooks
                                          org-install-agenda-files-menu
                                          org-latex-preview)
    (org-mode)))


;; Core type definition

(plist-define org-roam-note-filter
  :optional (:required :forbidden))

(defun org-roam-note-filter-parse (input)
  ;; (org-roam-note-filter-parse nil)
  ;; (org-roam-note-filter-parse "")
  ;; (org-roam-note-filter-parse "hello there")
  ;; (org-roam-note-filter-parse "-hello there +obi +wan")
  ;; (org-roam-note-filter-parse '(-hello there "+obi" "+wan"))
  (-let* ((tokens
           (cond
            ((null input) nil)
            ((stringp input)
             (split-string input " " t))
            ((symbolp input)
             (list (symbol-name input)))
            ((listp input)
             (seq-map (lambda (it) (format "%s" it)) input))
            (t
             (error "Cannot parse as note filter: %s" input))))
          ((forbidden required) (-separate (lambda (it) (string-prefix-p "-" it)) tokens)))
    (org-roam-note-filter-create :forbidden (seq-map (lambda (it) (string-remove-prefix "-" it))
                                                     forbidden)
                                 :required (seq-map (lambda (it) (string-remove-prefix "+" it))
                                                    required))))

(plist-define org-roam-note
  :required (:id :title :file)
  :optional (:tags :local-tags :next-review :last-review :maturity
             :todo-keywords :created :level :file-id))

(defvar org-roam-note-last-filter nil)

(defun org-roam-note-ignored-p (note &optional filter-plist)
  (let* ((filter-plist (or filter-plist org-roam-note-last-filter))
         (tags (org-roam-note-tags note))
         (forbidden-tags (org-roam-note-filter-forbidden filter-plist))
         (required-tags (org-roam-note-filter-required filter-plist)))
    (or (seq-intersection tags forbidden-tags)
        (seq-difference required-tags tags))))

(defun org-roam-notes-from-nodes (nodes &optional all)
  (->> nodes
       (seq-mapcat (lambda (node)
                     (when-let* ((file (org-roam-node-file node)))
                       (with-temp-buffer
                         (insert-file-contents file)
                         (org-roam-note--hacky-org-mode-enable)
                         (org-roam-notes-from-buffer (current-buffer) file all)))))
       (seq-uniq)
       (seq-remove #'org-roam-note-ignored-p)))

(defun org-roam-notes-from-backlinks (backlinks &optional all)
  (org-roam-notes-from-nodes (seq-map #'org-roam-backlink-source-node backlinks)
                             all))


;;; Define cache operations

;; Maintain a cache file to ensure review sessions are as responsive as
;; possible.

;; Define plumbing commands for cache here.

(defvar org-roam-note--cache nil)

(defun org-roam-note--cache ()
  (unless org-roam-note--cache
    (setq org-roam-note--cache
          (or (ignore-errors (ht-from-alist (read (f-read-text org-roam-note-cache-file))))
              (make-hash-table :test #'equal))))
  org-roam-note--cache)

(defun org-roam-note-from-id (id)
  (gethash id (org-roam-note--cache)))

(defun org-roam-note--cache-mutate (fn)
  (let ((cache (org-roam-note--cache)))
    (funcall fn cache)
    (f-write-text (prin1-to-string (ht-to-alist cache)) 'prefer-utf-8 org-roam-note-cache-file)
    cache))

(defun org-roam-note--cache-clear ()
  (setq org-roam-note--cache nil)
  (when (file-exists-p org-roam-note-cache-file)
    (delete-file org-roam-note-cache-file)))


;; Define cache-management porcelain in terms of plumbing.

(defun org-roam-note--cache-skip-note-p (file)
  (cl-assert file)
  (org-with-wide-buffer
   (save-match-data
     (or (org-entry-get-with-inheritance "REVIEW_EXCLUDED")
         (org-roam-note--daily-note-p file)
         (seq-intersection org-roam-note-ignored-tags (org-roam-note-file-or-headline-tags))))))

(defun org-roam-note--todo-keywords-in-buffer ()
  (save-excursion
    (save-match-data
      (let ((acc)
            (case-fold-search))
        (goto-char (point-min))
        (while (search-forward-regexp org-todo-regexp nil t)
          (push (match-string-no-properties 1) acc))
        (seq-uniq acc)))))

(defun org-roam-note--buffer-title ()
  (org-with-wide-buffer
   (save-match-data
     (goto-char (point-min))
     (when (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol) nil t)
       (match-string-no-properties 1)))))

(defun org-roam-note-at-point (&optional file)
  (when-let* ((id (org-entry-get-with-inheritance "ID")))
    (org-with-wide-buffer
     ;; Make sure we're sitting on the declaration of the note.
     (goto-char (org-find-property "ID" id))

     (org-roam-note-create
      :id id
      :file (or file (buffer-file-name))
      :file-id (org-entry-get (point-min) "ID")
      :level (if (org-before-first-heading-p)
                 0
               (car (org-heading-components)))
      :todo-keywords (org-roam-note--todo-keywords-in-buffer)
      :next-review (-some->> (org-entry-get-with-inheritance "NEXT_REVIEW") (ts-parse-org))
      :last-review (-some->> (org-entry-get-with-inheritance "LAST_REVIEW") (ts-parse-org))
      :created (-some->> (org-entry-get-with-inheritance "CREATED") (ts-parse-org))
      :maturity (org-entry-get-with-inheritance "MATURITY")
      :title (substring-no-properties (or (org-get-heading t t t) (org-roam-note--buffer-title)))
      :local-tags (org-roam-note-file-or-headline-tags 'local)
      :tags (org-roam-note-file-or-headline-tags)))))

(defun org-roam-note-from-node (node)
  (let* ((org-inhibit-startup t)
         (file (org-roam-node-file node))
         (already-open-p (get-file-buffer file))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (save-excursion
            (goto-char (org-roam-node-point node))
            (org-roam-note-at-point)))

      (unless already-open-p
        (kill-buffer buf)))))

(defun org-roam-note-to-node (note)
  (org-roam-node-from-id (org-roam-note-id note)))

(defun org-roam-notes-from-buffer (buf file &optional all)
  (with-current-buffer  buf
    (org-with-wide-buffer
     (save-match-data
       (let ((acc))
         (goto-char (point-min))
         (while (search-forward-regexp (org-re-property "ID") nil t)
           (unless (and (not all) (org-roam-note--cache-skip-note-p file))
             (when-let* ((note (org-roam-note-at-point file)))
               (push note acc))))
         (nreverse acc))))))

(defun org-roam-note-excluded-note-ids-from-buffer (buf file)
  (with-current-buffer buf
    (org-with-wide-buffer
     (save-match-data
       (goto-char (point-min))
       (let ((acc))
         (while (search-forward-regexp (org-re-property "ID") nil t)
           (when (org-roam-note--cache-skip-note-p file)
             (let ((id (match-string-no-properties 3)))
               (push id acc))))
         (nreverse acc))))))

(defun org-roam-note--update-by-props-in-buffer (cache buf file)
  (dolist (note (org-roam-notes-from-buffer buf file))
    (puthash (org-roam-note-id note) note cache))
  (dolist (id (org-roam-note-excluded-note-ids-from-buffer buf file))
    (remhash id cache)))

(defun org-roam-note--daily-note-p (&optional file)
  "Test whether FILE is a daily note.

If FILE is not given, checks the current buffer.

This is a wrapper that makes sure org-roam-directory is well-formed.

See:
https://github.com/org-roam/org-roam/issues/2032"
  (cl-assert (or file (buffer-file-name)))
  (let ((org-roam-directory (string-remove-suffix org-roam-dailies-directory org-roam-directory)))
    (org-roam-dailies--daily-note-p file)))

(defun org-roam-note--cache-update ()
  "Update the evergreen notes cache from `after-save-hook'."
  (when (and (derived-mode-p 'org-mode)
             (not (org-roam-note--daily-note-p (buffer-file-name))))
    (org-roam-note--cache-mutate (lambda (cache)
                                   (org-roam-note--update-by-props-in-buffer cache
                                                                             (current-buffer)
                                                                             (buffer-file-name))))))

(defun org-roam-note-cache-collect (fn)
  (let ((table (copy-hash-table (org-roam-note--cache))))
    (maphash (lambda (key note)
               (if-let* ((result (funcall fn note)))
                   (puthash key result table)
                 (remhash key table)))
             table)
    (hash-table-values table)))

(defun org-roam-note-file-or-headline-tags (&optional local)
  (seq-map #'substring-no-properties
           (if (org-before-first-heading-p)
               org-file-tags
             (org-get-tags nil local))))

;;;###autoload
(defun org-roam-note-cache-rebuild ()
  "Rebuild the evergreen notes cache."
  (interactive)
  (org-roam-note--cache-clear)
  (let* ((files
          (let* ((msg (format "Scanning files in %s ..." (abbreviate-file-name org-roam-directory)))
                 (reporter (make-progress-reporter msg)))
            (prog1 (f-files org-roam-directory
                            (lambda (file)
                              (when (f-ext-p file "org")
                                (progress-reporter-update reporter)
                                t))
                            t)
              (progress-reporter-done reporter)))))

    (let ((reporter (make-progress-reporter "Parsing notes..." 0 (length files)))
          (n 0))
      (dolist (file files)
        (cl-incf n)
        (progress-reporter-update reporter n file)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (org-roam-note--hacky-org-mode-enable)
          (unless (org-roam-note--cache-skip-note-p file)
            (org-roam-note--update-by-props-in-buffer (org-roam-note--cache)
                                                      (current-buffer)
                                                      file))))

      (progress-reporter-done reporter))
    ;; Write back to disk.
    (org-roam-note--cache-mutate #'ignore))
  (message "Rebuilt evergreen notes index."))

;;;###autoload
(defun org-roam-note-cache-show ()
  "Show the contents the evegreen notes cache for debugging."
  (interactive)
  (pp-display-expression (ht-to-plist (org-roam-note--cache))
                         "*org-roam-note cache*"))

;;;###autoload
(define-minor-mode org-roam-note-cache-mode
  "Minor mode to enable book-keeping used for notes reviews."
  :group 'org-roam-note
  (if org-roam-note-cache-mode
      (add-hook 'after-save-hook #'org-roam-note--cache-update nil t)
    (remove-hook 'after-save-hook #'org-roam-note--cache-update t)))

(provide 'org-roam-note)

;;; org-roam-note.el ends here
