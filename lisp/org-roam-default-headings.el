;;; org-roam-default-headings.el --- Create a default set of headlines for nodes  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Chris Barrett

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
(require 'org-format)
(require 'org-funcs)
(require 'org-roam)

(defgroup org-roam-default-headings nil
  "Create a default set of headings for all nodes."
  :group 'productivity
  :prefix "org-roam-default-headings-")

(defcustom org-roam-default-headings-function #'org-roam-default-headings-for-node
  "A function that takes a node and returns a list of default heading names.

The function should return a list, where each item is a heading
in the order it should appear in the file.

Each item in the list may be either:

1. A heading name as a string

2. A plist, with the following keys:

   - :name (required) - the name of the heading

   - :ensure - whether the heading should be created if it does
     not exist. Default is true.

   - :dblock - a dynamic block that should be present under the
     heading. The value for this key should be a dblock header
     plist."
  :group 'org-roam-default-headings
  :type 'function)

(defun org-roam-default-headings-for-node (node)
  (cond
   ((seq-intersection (org-roam-node-tags node) '("dailies" "doc")) nil)
   ((seq-contains-p (org-roam-node-tags node) "litnotes")
    '("Meta"
      ("Topics" :ensure nil)
      "Related"
      ("References" :dblock (:name "backlinks" :tags litnotes :only-missing t))
      "Notes"
      ("Footnotes" :ensure nil)))
   ((seq-contains-p (org-roam-node-tags node) "runbook")
    '(("Notes" :ensure nil)
      ("Topics" :ensure nil)
      ("Tasks" :ensure nil)
      ("Related" :ensure nil)
      ("References" :ensure nil :dblock (:name "backlinks" :tags litnotes :only-missing t))
      ("Footnotes" :ensure nil)))
   (t
    '("Notes"
      "Topics"
      ("Tasks" :ensure nil)
      "Related"
      ("References" :dblock (:name "backlinks" :tags litnotes :only-missing t))
      ("Planning & Meetings" :tags ("outline") :ensure nil)
      ("Footnotes" :ensure nil)))))

(defun org-roam-default-headings--find-or-create-heading (heading)
  (or (org-find-exact-headline-in-buffer heading)
      (progn
        ;; Create heading if it doesn't exist
        (goto-char (point-max))
        (unless (bolp) (newline))
        (let (org-insert-heading-respect-content)
          (org-insert-heading nil nil t))
        (insert heading))))

;;;###autoload
(defun org-roam-default-headings-populate (&optional node)
  "Populate the current roam NODE with headings."
  (interactive)
  (when (org-roam-file-p)
    (org-with-wide-buffer
     (when-let* ((node (or node
                           (save-excursion
                             (goto-char (point-min))
                             (org-roam-node-at-point))))
                 (heading-specs (--map (append (-list it) (list :ensure t :dblock nil))
                                       (funcall org-roam-default-headings-function node))))
       (-each heading-specs
         (-lambda ((name &plist :ensure :dblock :tags))
           (when-let* ((marker (if ensure
                                   (org-roam-default-headings--find-or-create-heading name)
                                 (org-find-exact-headline-in-buffer name))))
             (org-with-point-at marker
               (org-set-tags tags)
               (org-funcs-move-headline-to-end)
               (when dblock (org-funcs-ensure-dblock-for-heading-at-pt dblock)))
             (set-marker marker nil))))

       (org-format-all-headings)))))

;;;###autoload
(defun org-roam-default-headings-populate-for-find-file ()
  (when (org-roam-file-p)
    (let ((initial-contents (buffer-substring-no-properties (point-min) (point-max)))
          (updated-contents
           (progn
             (org-roam-default-headings-populate)
             (buffer-substring-no-properties (point-min) (point-max)))))
      (if (equal initial-contents updated-contents)
          (set-buffer-modified-p nil)
        (save-buffer)))))

(provide 'org-roam-default-headings)

;;; org-roam-default-headings.el ends here
