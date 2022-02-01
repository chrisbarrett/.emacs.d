;;; org-roam-links.el --- Buffer showing links in an org-roam note -*- lexical-binding: t; -*-

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

;; org-roam comes with a backlanks buffer that shows previous of other locations
;; in the Zettlekasten linking here. This is useful for most cases, but it would
;; be useful to see another level of links deep to find unexpected note
;; connections.

;;; Code:

(require 'dash)
(require 'org-roam-review)

(defun org-roam-links-get (node)
  (org-with-point-at (org-id-find (org-roam-node-id node) 'marker)
    (when (org-at-heading-p) (org-narrow-to-subtree))
    (let (ids)
      (save-match-data
        (while (search-forward-regexp (rx "[[id:" (group (+? nonl)) "]") nil t)
          (push (match-string-no-properties 1) ids)))

      (seq-mapcat (lambda (it) (ignore-errors
                            (when-let* ((node (org-roam-node-from-id it)))
                              (list node))))
                  (nreverse ids)))))

(plist-define org-roam-links-graph
  :required (:nodes :tree))

(defun org-roam-links-graph (node depth)
  "Return the linked nodes, and their links, up to DEPTH."
  (when (cl-plusp depth)
    (let ((linked-nodes (append
                         (seq-map #'org-roam-backlink-source-node (org-roam-backlinks-get node))
                         (org-roam-links-get node)))
          (nodes (ht-create))
          (tree (ht-create)))
      (dolist (node linked-nodes)
        (let ((id (org-roam-node-id node))
              (children (org-roam-links-graph node (1- depth))))
          (puthash id node nodes)
          (-when-let ((&plist :nodes child-nodes) children)
            (setq nodes (ht-merge nodes child-nodes)))
          (puthash (org-roam-node-id node) (plist-get children :tree) tree)))
      (org-roam-links-graph-create :nodes nodes :tree tree))))

(defconst org-roam-links-max-title-length 50)

;;;###autoload
(defun org-roam-links-view (&optional depth)
  "Show Evergreen Note links for the current buffer.

When called interactively, prompt the user for DEPTH."
  (interactive (when current-prefix-arg (list (read-number "Depth: " 2))))
  (-let* ((start-node (or (org-roam-node-at-point)
                          (org-roam-node-read)))
          (title (org-roam-node-title start-node))
          (short-title (substring title 0 (min (length title) org-roam-links-max-title-length)))
          (short-title (if (equal title short-title) title (concat short-title "…")))
          (depth (or depth 2))
          graph)
    (org-roam-review-display-buffer-and-select
     (org-roam-review-create-buffer
      :title (format "Links for “%s\”" short-title)
      :instructions "Below is the graph of links to and from the current node."
      :placeholder "No linked notes"
      :buffer-name "*org-roam-links*"
      :notes
      (lambda ()
        (setq graph (org-roam-links-graph start-node depth))
        (seq-filter (lambda (note)
                      (not (org-roam-review-note-ignored-p note)))
                    (org-roam-review-notes-from-nodes (ht-values (org-roam-links-graph-nodes graph)))))
      :insert-notes-fn
      (-lambda ((&plist :root :notes))
        (let ((seen-ids (ht-create))
              (nodes (org-roam-links-graph-nodes graph))
              (tree (org-roam-links-graph-tree graph)))
          (puthash (org-roam-node-id start-node) t seen-ids)
          (cl-labels ((render-at-depth
                       (tree depth)
                       (maphash (lambda (id children)
                                  (when-let* ((node (ht-get nodes id))
                                              (note-exists-p (seq-find (lambda (it) (equal id (org-roam-review-note-id it))) notes)))
                                    (magit-insert-section section (org-roam-preview-section)
                                      (oset section parent root)
                                      (oset section point (org-roam-node-point node))
                                      (oset section file (org-roam-node-file node))
                                      (let* ((seen-p (gethash id seen-ids))
                                             (face
                                              (cond
                                               ((zerop depth) 'magit-section-heading)
                                               (seen-p 'font-lock-comment-face)
                                               (t 'magit-section-secondary-heading)))
                                             (heading (propertize (org-roam-node-title node) 'font-lock-face face)))

                                        (magit-insert-heading (org-roam-review-indent-string heading depth))
                                        (unless seen-p
                                          (puthash id t seen-ids)
                                          (when children
                                            (render-at-depth children (1+ depth))))))))
                                tree)))
            (render-at-depth tree 0))))))))


(provide 'org-roam-links)

;;; org-roam-links.el ends here
