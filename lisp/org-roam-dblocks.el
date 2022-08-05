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


;;; Backlinks dblock type

;;;###autoload
(defun org-dblock-write:backlinks (params)
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
          (insert (string-join lines  "\n"))))
    (error (insert (error-message-string err)))))

;;;###autoload
(defun org-insert-dblock:backlinks ()
  "Insert a backlinks dynamic block at point."
  (interactive)
  (atomic-change-group
    (org-create-dblock (list :name "backlinks")))
  (org-update-dblock))


;;; Roam notes search dblock type

;;;###autoload
(defun org-dblock-write:notes (params)
  (condition-case err
      (progn
        (org-roam-dblocks-args-assert params t)
        (cl-assert (or (org-roam-dblocks-args-match params) (org-roam-dblocks-args-tags params)) t "Must provide at least one of :tags or :match")
        (-let* ((backlinks (->> (org-roam-node-list)
                                (-keep (org-roam-dblocks--compiled-predicates params))
                                (seq-sort 'org-roam-dblocks--node-sorting)))
                (lines (seq-map 'org-roam-dblocks--node-to-link backlinks)))
          (insert (string-join lines  "\n"))))
    (error (insert (error-message-string err)))))

;;;###autoload
(defun org-insert-dblock:notes ()
  "Insert a backlinks dynamic block at point."
  (interactive)
  (let ((args (pcase-exhaustive (read-char-choice "Query type:  [t] title match (regexp)\n             [k] tags" '(?t ?k))
                (?t
                 (list :match (read-string "Match title (regexp): ")))
                (?k
                 (list :tags (org-roam-note-filter-read))))))
    (atomic-change-group
      (org-create-dblock (append '(:name "notes") args))))
  (org-update-dblock))



(defconst org-roam-dblocks-names '("notes" "backlinks"))

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
