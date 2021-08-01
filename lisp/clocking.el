;;; clocking.el --- Functions for managing clocking and timekeeping  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Chris Barrett

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

;; Adapted from http://doc.norang.ca/org-mode.html#Clocking

;;; Code:

(require 'ht)
(require 'subr-x)

(autoload 'org-agenda-remove-restriction-lock "org-agenda")
(autoload 'org-clock-in "org-clock")
(autoload 'org-clock-in-last "org-clock")
(autoload 'org-clock-out "org-clock")
(autoload 'org-clocking-p "org-clock")
(autoload 'org-find-exact-headline-in-buffer "org")
(autoload 'org-get-heading "org")
(autoload 'org-insert-heading "org")
(autoload 'org-roam-node--find "org-roam")
(autoload 'org-roam-node-file "org-roam")
(autoload 'org-roam-node-id "org-roam")
(autoload 'org-roam-node-list "org-roam")
(autoload 'org-roam-node-title "org-roam")
(autoload 'org-save-all-org-buffers "org")
(autoload 'org-with-point-at "org-macs" nil nil 'macro)

(defgroup clocking nil
  "Functions for managing client timekeeping with org-clock."
  :group 'productivity
  :prefix "clocking-")

(defcustom clocking-clients-alist nil
  "Alist mapping org tags to IDs.

Tags are expected to be file tags, applied to todos for that
client. The ID is the org-roam node for the client's notes file."
  :group 'clocking
  :type '(alist :key-type string :value-type string))

(defcustom clocking-default-headline-name "Planning & Meetings"
  "The name of the heading to clock in to if not working on a specific task.

The heading will be created if needed."
  :group 'clocking
  :type 'string)

(defvar clocking-agenda-should-update-hook nil
  "Hook run when a clocking change should update the agenda.")

(defun clocking--ensure-clocking-headline (buffer)
  "Create the default heading for clocking in BUFFER.

Return the position of the headline."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (if-let* ((marker (org-find-exact-headline-in-buffer clocking-default-headline-name)))
            (marker-position marker)
          (goto-char (point-max))
          (delete-horizontal-space)
          (org-insert-heading nil nil t)
          (insert clocking-default-headline-name)
          (point))))))

(defun clocking--client-node-ids ()
  (seq-map 'cdr clocking-clients-alist))

(defun clocking--choose-tag-for-node-id (id)
  (or (car (seq-find (lambda (it) (equal (cdr it) id))
                     clocking-clients-alist))
      (completing-read "Tag:" (seq-map 'car clocking-clients-alist))))

(defvar clocking--last-client-choice nil)

(defun clocking--client-nodes ()
  (let ((nodes (ht-values (seq-reduce (lambda (acc it)
                                        (let ((id (org-roam-node-id it)))
                                          (when (member id (clocking--client-node-ids))
                                            (ht-set acc id it)))
                                        acc)
                                      (org-roam-node-list)
                                      (ht-create)))))
    (ht-from-alist (seq-map (lambda (node)
                              (cons (org-roam-node-title node) node))
                            nodes))))

(defun clocking-client-files ()
  (seq-map #'org-roam-node-file (ht-values (clocking--client-nodes))))

(defun clocking-find-client-buffer (&optional arg)
  "Open the org-roam buffer for the current client.

With prefix arg ARG, prompt for the client to open."
  (interactive "P")
  (switch-to-buffer
   (org-roam-node--find
    (cond
     (arg
      (clocking--choose-client-node))
     (clocking--last-client-choice
      (clocking--get-node-by-name clocking--last-client-choice))
     (t
      (clocking--choose-client-node))))))

(defun clocking-work-tag ()
  (let ((node
         (if clocking--last-client-choice
             (clocking--get-node-by-name clocking--last-client-choice)
           (clocking--choose-client-node))))
    (clocking--choose-tag-for-node-id (org-roam-node-id node))))

(defun clocking-heading-function ()
  (let ((headline (substring-no-properties (org-get-heading t t t t))))
    (format "%s/%s"
            clocking--last-client-choice
            headline)))

(defun clocking--choose-client-node ()
  (let* ((nodes (clocking--client-nodes))
         (choice (completing-read "Client: " (ht-keys nodes) nil t nil 'clocking--choose-buffer-history
                                  clocking--last-client-choice)))
    (setq clocking--last-client-choice choice)
    (ht-get nodes choice)))

(defun clocking--get-node-by-name (name)
  (ht-get (clocking--client-nodes) name))

(defun clocking--punch-in-for-node (node)
  (with-current-buffer (org-roam-node--find node)
    (org-with-point-at (clocking--ensure-clocking-headline (current-buffer))
      (org-clock-in '(16)))))

(defun clocking-punch-in (&optional arg)
  "Punch in with the default date tree.

Remembers the last client chosen. If prefix ARG is given, prompt
for the client to use."
  (interactive "P")
  (cond
   (arg
    (clocking--punch-in-for-node (clocking--choose-client-node)))
   ((and (boundp 'org-clock-history) (null org-clock-history))
    (let ((node (if (and arg clocking--last-client-choice)
                    (clocking--get-node-by-name clocking--last-client-choice)
                  (clocking--choose-client-node))))
      (clocking--punch-in-for-node node)))
   (t
    (call-interactively #'org-clock-in-last)))

  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'clocking-agenda-should-update-hook)))

(defun clocking-punch-out ()
  "Stop the clock."
  (interactive)
  (when (org-clocking-p)
    (org-clock-out))
  (org-agenda-remove-restriction-lock)
  (org-save-all-org-buffers)
  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'clocking-agenda-should-update-hook))
  (message "Punched out."))

(provide 'clocking)

;;; clocking.el ends here
