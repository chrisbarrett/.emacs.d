;;; org-roam-rewrite.el --- Commands for rewriting org-roam nodes and their links.  -*- lexical-binding: t; -*-

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

;; Provides commands for rewriting links and removing nodes in a structured way,
;; to reduce the likelihood of leaving broken links in your org-roam files.

;;; Code:

(require 'f)
(require 'org)
(require 'org-roam)

(defgroup org-roam-rewrite nil
  "Commands for rewriting org-roam nodes and their links."
  :group 'productivity
  :prefix "org-roam-rewrite-")

(defcustom org-roam-rewrite-extract-excluded-tags '("ATTACH")
  "Tags that should not be propagated when extracting notes."
  :group 'org-roam-rewrite
  :type '(repeat string))



(defun org-roam-rewrite--set-title-keyword (text)
  (org-with-wide-buffer
   (goto-char (point-min))
   (save-match-data
     (search-forward-regexp (rx bol "#+title:" (* space) (group (+ any)) eol))
     (replace-match text t nil nil 1))))

(defun org-roam-rewrite--set-file-tags (tags)
  (org-with-wide-buffer
   (goto-char (point-min))
   (unless (search-forward-regexp (rx bol "#+filetags:" (group (* nonl))) nil t)
     (cond ((search-forward-regexp (rx bol "#+title:"))
            (goto-char (line-end-position))
            (insert "\n#+filetags:"))
           (t
            (insert "#+filetags:\n"))))

   (let ((formatted (if tags
                        (format ":%s:" (string-join tags ":"))
                      "")))
     (save-match-data
       (goto-char (point-min))
       (when (search-forward-regexp (rx bol "#+filetags:" (group (* nonl))))
         (replace-region-contents (match-beginning 1) (match-end 1)
                                  (lambda ()
                                    (concat " " formatted))))))))

(defun org-roam-rewrite--file-tags ()
  (save-match-data
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (search-forward-regexp (rx bol "#+filetags:" (group (+ nonl)))
                                  nil
                                  t)
       (split-string (string-trim (substring-no-properties (match-string 1))) ":" t)))))



(defun org-roam-rewrite--edit-backlinks (backlinks new-id new-title)
  (let ((replacement (org-link-make-string (concat "id:" new-id) new-title))
        (backlinks-by-file
         (seq-group-by (-compose #'org-roam-node-file #'org-roam-backlink-source-node)
                       backlinks)))
    (pcase-dolist (`(,file . ,backlinks) backlinks-by-file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (backlink (seq-sort-by #'org-roam-backlink-point #'> backlinks))
          (goto-char (org-roam-backlink-point backlink))
          (save-match-data
            (looking-at org-link-any-re)
            (replace-match replacement t t)))
        (write-region (point-min) (point-max) file)))))

(defun org-roam-rewrite--update-node-title (node new-title)
  (org-id-goto (org-roam-node-id node))
  (cond ((equal 0 (org-roam-node-level node))
         (org-roam-rewrite--set-title-keyword new-title))
        ((looking-at org-complex-heading-regexp)
         (replace-match new-title t t nil 4)))
  (save-buffer))

(defun org-roam-rewrite--delete-node-and-buffer (node)
  (when-let* ((buf (find-buffer-visiting (org-roam-node-file node))))
    (kill-buffer buf))
  (delete-file (org-roam-node-file node)))

;;;###autoload
(defun org-roam-rewrite-rename (node new-title)
  "Change the title of a note and update links to match.

NODE is the node to update.

NEW-TITLE is the new title to use. All backlinks will have their
descriptions updated to this value."
  (interactive (let* ((node (org-roam-node-read (-some->> (org-roam-node-at-point) (org-roam-node-title))
                                                nil nil t "Rename: ")))
                 (list node (read-string "New title: " (org-roam-node-title node)))))
  (org-roam-node-visit node)
  (org-save-all-org-buffers)
  (let ((backlinks (org-roam-backlinks-get node)))
    (cond
     ((null backlinks)
      (org-roam-rewrite--update-node-title node new-title)
      (message "No backlinks found."))
     ((y-or-n-p (format "Rewriting %s link(s) from \"%s\" -> \"%s\". Continue? "
                        (length backlinks) (org-roam-node-title node) new-title))
      (org-roam-rewrite--update-node-title node new-title)
      (org-roam-rewrite--edit-backlinks backlinks (org-roam-node-id node) new-title)
      (message "Rewrote %s links to note." (length backlinks)))
     (t
      (user-error "Rewrite aborted")))))

;;;###autoload
(defun org-roam-rewrite-remove (from to link-desc)
  "Redirect links from one node to a replacement node.

Optionally, delete the original node after all links are
redirected.

FROM is the node which will be unlinked.

TO is the node to change those references to point to.

LINK-DESC is the description to use for the updated links."
  (interactive (let* ((from (org-roam-node-read nil nil nil t "Remove: "))
                      (to (org-roam-node-read nil (lambda (it) (not (equal from it))) nil t "Rewrite to: ")))
                 (list from to (read-string "Link description: " (org-roam-node-title to)))))
  (org-save-all-org-buffers)
  (let ((backlinks (org-roam-backlinks-get from)))
    (cond
     ((null backlinks)
      (when (y-or-n-p "No links found. Delete note? ")
        (org-roam-rewrite--delete-node-and-buffer from)))
     ((y-or-n-p (format "Rewriting %s link(s) from \"%s\" -> \"%s\". Continue? "
                        (length backlinks) (org-roam-node-title from) link-desc))
      (org-roam-rewrite--edit-backlinks backlinks (org-roam-node-id to) link-desc)
      (when (y-or-n-p "Rewrite completed. Delete note? ")
        (org-roam-rewrite--delete-node-and-buffer from)))
     (t
      (user-error "Rewrite aborted")))))

;;;###autoload
(defun org-roam-rewrite-inline (src-node dest-node)
  "Inline the contents of one org-roam note into another, removing the original.

SRC-NODE is the node to be removed.

DEST-NODE is the node that will be added to."
  (interactive
   (let* ((src (org-roam-node-read (-some->> (org-roam-node-at-point) (org-roam-node-title)) nil nil t "Source: "))
          (dest (org-roam-node-read nil (lambda (node)
                                          (and
                                           (not (equal (org-roam-node-id node) (org-roam-node-id src)))
                                           (zerop (org-roam-node-level node))
                                           (not (seq-contains-p (org-roam-node-tags node) "dailies"))))
                                    nil t "Destination: ")))
     (list src dest)))

  (let* ((org-inhibit-startup t)
         (src-buffer (find-file-noselect (org-roam-node-file src-node)))
         (content
          (with-current-buffer src-buffer
            (org-with-wide-buffer
             (org-transclusion-remove-all)
             (goto-char (point-min))
             (org-roam-end-of-meta-data t)
             (buffer-substring (point) (point-max))))))
    (find-file (org-roam-node-file dest-node))
    (org-with-wide-buffer
     (org-transclusion-remove-all)
     (goto-char (point-max))
     (delete-blank-lines)
     (insert "\n\n")
     (insert (format "* %s\n" (org-roam-node-title src-node)))
     (org-set-property "ID" (org-roam-node-id src-node))
     (save-restriction
       (narrow-to-region (point) (point-max))
       (insert content)
       (org-map-entries 'org-do-demote)
       (goto-char (point-min))
       (while (search-forward-regexp (rx bol "#+transclude:") nil t)
         (org-transclusion-add)
         (org-transclusion-promote-subtree))))
    (delete-file (org-roam-node-file src-node))
    (save-buffer)
    (org-transclusion-add-all)
    (when (buffer-live-p src-buffer)
      (kill-buffer src-buffer)))

  (org-roam-node-visit dest-node)
  (message "Inlined note successfully"))

;;;###autoload
(defun org-roam-rewrite-extract ()
  "Convert current subtree at point to a node, and extract it into a new file.

It's a re-implementation of `org-roam-extract-subtree', but
handles file titles, IDs and tags better."
  (interactive)
  (let ((dest-file (f-join org-roam-directory "notes" (format-time-string "%Y-%m-%d--%H-%M-%S.org"))))
    (save-excursion
      (when (org-before-first-heading-p)
        (user-error "Already a top-level node"))

      (save-buffer)
      (org-roam-db-update-file)
      (when (file-exists-p dest-file)
        (user-error "%s exists. Aborting" dest-file))

      (atomic-change-group
        (let ((title (org-get-heading))
              (tags (org-get-tags))
              (id (org-id-get-create)))

          (save-restriction
            (org-narrow-to-subtree)
            (when (bound-and-true-p org-transclusion-mode)
              (org-transclusion-remove-all t)))

          (org-cut-subtree)
          (insert (org-link-make-string (format "id:%s" id) (org-link-display-format title)))
          (newline)
          (save-buffer)
          (f-touch dest-file)
          (with-current-buffer (find-file-noselect dest-file)
            (condition-case _
                (progn
                  (org-paste-subtree nil nil nil t)
                  (org-roam-promote-entire-buffer)
                  (org-roam-rewrite--set-title-keyword title)
                  (require 'org-roam-review)
                  (when-let* ((tags (-difference (-union (org-roam-rewrite--file-tags) tags)
                                                 org-roam-rewrite-extract-excluded-tags)))
                    (org-roam-rewrite--set-file-tags tags)
                    (when (bound-and-true-p org-transclusion-mode)
                      (org-transclusion-add-all)))
                  (save-buffer))
              (error
               (f-delete dest-file)))

            (run-hook-with-args 'org-roam-capture-new-node-hook)))))))

(provide 'org-roam-rewrite)

;;; org-roam-rewrite.el ends here
