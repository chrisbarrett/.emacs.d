;;; org-roam-dblocks.el --- Defines a dynamic block type in org-mode for backlinks.  -*- lexical-binding: t; -*-

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

;; Defines dynamic block types for use with org-roam. Example configuration:
;;
;;    (use-package org-roam-dblocks
;;      :hook (org-mode . org-roam-dblocks-autoupdate-mode)
;;      :custom
;;      (org-roam-dblocks-auto-refresh-tags '("moc")))
;;
;;
;; The "backlinks" block type writes a list of backlinks into the block body.
;;
;; The "notes" block type searches for notes based on the header criteria and
;; inserts links into the block body.
;;
;; These dynamic blocks can optionally be updated when opening and saving
;; buffers.

;;; Code:

(require 'dash)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-roam-note)
  (require 'org-roam))

(require 'plist)

(defgroup org-roam-dblocks nil
  "Adds support for a dynamic block of org-roam backlinks to `org-mode'."
  :group 'productivity
  :prefix "org-roam-dblocks-")

(defcustom org-roam-dblocks-auto-refresh-tags nil
  "A list of tags (as strings) or nil.

If non-nil, only org-roam nodes with the specified tags have
their blocks are updated automatically."
  :group 'org-roam-dblocks
  :type '(choice (const nil)
                 (repeat :tag "Tag" (string))))

(defconst org-roam-dblocks-names '("notes" "backlinks"))



(plist-define org-roam-dblocks-args
  :optional (:id :match :tags
             :name :indentation-column :content))

(defun org-roam-dblocks--node-to-link (node)
  (let ((link (concat "id:" (org-roam-node-id node)))
        (desc (org-roam-node-title node)))
    (concat "- " (org-link-make-string link desc))))

(defun org-roam-dblocks--parse-regexp-form (form)
  ;;; Quick tests:
  ;; (org-roam-dblocks--parse-regexp-form nil)
  ;; (org-roam-dblocks--parse-regexp-form 'hi)
  ;; (org-roam-dblocks--parse-regexp-form "hi")
  ;; (org-roam-dblocks--parse-regexp-form '(rx bol "hi" eol))
  (cond
   ((null form) nil)
   ((stringp form)
    (unless (zerop (length form))
      form))
   ((symbolp form)
    (symbol-name form))
   (t
    (pcase form
      (`(rx . ,args)
       (rx-to-string (cons 'and args)
                     t))))))

(defun org-roam-dblocks--eval-regexp-predicate (node match)
  (or (null match)
      (string-match-p match (org-roam-node-title node))))

(defun org-roam-dblocks--eval-tags-predicate (node tags-filter)
  (let* ((tags (org-roam-node-tags node))
         (forbidden-tags (org-roam-note-filter-forbidden tags-filter))
         (required-tags (org-roam-note-filter-required tags-filter)))
    (not (or (seq-intersection tags forbidden-tags)
             (seq-difference required-tags tags)))))

(defalias 'org-roam-dblocks--node-sorting
  (-on #'string-lessp (-compose #'downcase #'org-roam-node-title)))

(defun org-roam-dblocks--compiled-predicates (params)
  (-let ((tags (org-roam-note-filter-parse (org-roam-dblocks-args-tags params)))
         (match (org-roam-dblocks--parse-regexp-form (org-roam-dblocks-args-match params))))
    (lambda (node)
      (when (and (org-roam-dblocks--eval-regexp-predicate node match)
                 (org-roam-dblocks--eval-tags-predicate node tags))
        node))))


;; HACK: To avoid dirtying the buffer when blocks haven't changed, we actually
;; compute the data to insert earlier, at the phase where org would normally
;; blindly clear out the block's content. We don't clear the content

(defun org-roam-dblocks--prepare-dblock (fn &rest args)
  "Advice to hack org's dblock update flow for the dblock types we define.

Populates `org-roam-dblocks--content' and ensures the buffer
stays unchanged if there's no difference between the new content
and old content."
  (unless (looking-at org-dblock-start-re)
    (user-error "Not at a dynamic block"))
  (let ((name (org-no-properties (match-string 1))))
    (if (not (member name org-roam-dblocks-names))
        ;; Defer to default implementation for any dblocks we don't define in
        ;; this file..
        (apply fn args)
      (let* ((content-start (1+ (match-end 0)))
             (content-end)
             (params (append (list :name name)
                             (read (concat "(" (match-string 3) ")")))))
        (save-excursion
          (beginning-of-line 1)
          (skip-chars-forward " \t")
          (setq params (plist-put params :indentation-column (current-column))))

        (if (re-search-forward org-dblock-end-re nil t)
            (setq content-end (match-beginning 0))
          (error "Dynamic block not terminated"))

        (let* ((current-content (buffer-substring-no-properties content-start content-end))
               (updated-content
                (pcase-exhaustive name
                  ("notes" (org-roam-dblocks-format-notes params))
                  ("backlinks" (org-roam-dblocks-format-backlinks params))))

               (content-changed-p (not (equal (string-trim current-content) updated-content)))
               (params (append params (list :content current-content
                                            :new-content (and content-changed-p updated-content)))))

          ;; Only clear the block if the content should change.
          (when content-changed-p
            (delete-region content-start content-end)
            (goto-char content-start)
            (open-line 1))

          params)))))

(advice-add 'org-prepare-dblock :around #'org-roam-dblocks--prepare-dblock)

;;;###autoload
(defun org-roam-dblocks--write-content (params)
  "Inserts updated dblock content prepared by `org-roam-dblocks--prepare-dblock'."
  (when-let* ((new-content (plist-get params :new-content)))
    (insert new-content)))


;;; Backlinks dblock type

(defun org-roam-dblocks-format-backlinks (params)
  (condition-case err
      (progn
        (org-roam-dblocks-args-assert params t)
        (-let* ((id (org-roam-dblocks-args-id params))
                (node (if id (org-roam-node-from-id id) (org-roam-node-at-point t)))
                (backlinks (->>
                            (org-roam-backlinks-get node :unique t)
                            (-keep (-compose (org-roam-dblocks--compiled-predicates params) #'org-roam-backlink-source-node))
                            (seq-sort 'org-roam-dblocks--node-sorting)))
                (lines (seq-map 'org-roam-dblocks--node-to-link backlinks)))
          (string-join lines  "\n")))
    (error (error-message-string err))))

;;;###autoload
(defun org-insert-dblock:backlinks ()
  "Insert a backlinks dynamic block at point."
  (interactive)
  (atomic-change-group
    (org-create-dblock (list :name "backlinks")))
  (org-update-dblock))

;;;###autoload
(defalias 'org-dblock-write:backlinks #'org-roam-dblocks--write-content)


;;; Roam notes search dblock type

(defun org-roam-dblocks-format-notes (params)
  (condition-case err
      (progn
        (org-roam-dblocks-args-assert params t)
        (cl-assert (or (org-roam-dblocks-args-match params) (org-roam-dblocks-args-tags params)) t "Must provide at least one of :tags or :match")
        (-let* ((backlinks (->> (org-roam-node-list)
                                (-keep (org-roam-dblocks--compiled-predicates params))
                                (seq-sort 'org-roam-dblocks--node-sorting)))
                (lines (seq-map 'org-roam-dblocks--node-to-link backlinks)))
          (string-join lines  "\n")))
    (error (error-message-string err))))

;;;###autoload
(defun org-insert-dblock:notes ()
  "Insert a backlinks dynamic block at point."
  (interactive)
  (let ((args (pcase-exhaustive (completing-read "Query Type: " '("Title Regexp Match" "Tags Filter"))
                ("Title Regexp Match"
                 (list :match (read-string "Match title (regexp): ")))
                ("Tags Filter"
                 (list :tags (org-roam-note-filter-read))))))
    (atomic-change-group
      (org-create-dblock (append '(:name "notes") args))))
  (org-update-dblock))

;;;###autoload
(defalias 'org-dblock-write:notes #'org-roam-dblocks--write-content)



(defun org-roam-dblocks--update-block-at-point-p ()
  (or (null org-roam-dblocks-auto-refresh-tags)
      (seq-intersection org-roam-dblocks-auto-refresh-tags
                        (append org-file-tags (org-get-tags)))))

(defun org-roam-dblocks--update-blocks ()
  (org-map-dblocks
   (lambda ()
     (when (org-roam-dblocks--update-block-at-point-p)
       (pcase (org-element-at-point)
         (`(dynamic-block ,plist)
          (when (member (plist-get plist :block-name) org-roam-dblocks-names)
            (org-update-dblock))))))))



;;;###autoload
(define-minor-mode org-roam-dblocks-autoupdate-mode
  "Automatically update backlinks blocks on visit and save."
  :init-value nil
  (cond
   (org-roam-dblocks-autoupdate-mode
    (add-hook 'before-save-hook #'org-roam-dblocks--update-blocks nil t)
    (org-roam-dblocks--update-blocks))
   (t
    (remove-hook 'before-save-hook #'org-roam-dblocks--update-blocks))))

(with-eval-after-load 'org
  (org-dynamic-block-define "notes" #'org-insert-dblock:notes)
  (org-dynamic-block-define "backlinks" #'org-insert-dblock:backlinks))

(provide 'org-roam-dblocks)

;;; org-roam-dblocks.el ends here
