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

(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-roam)

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
  (let ((headline (nth 4 (org-heading-components))))
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

(defvar clocking--session-active-p nil)

(defun clocking--clock-in-on-default (&optional prompt-for-client)
  (clocking--punch-in-for-node
   (if (or prompt-for-client (null clocking--last-client-choice))
       (clocking--choose-client-node)
     (clocking--get-node-by-name clocking--last-client-choice))))

(defun clocking-punch-in (&optional arg)
  "Punch in with the default date tree for a client.

Remembers the last client chosen.

With a `\\[universal-argument]' prefix argument ARG, clock into \
the task at point.

When ARG is `\\[universal-argument] \ \\[universal-argument]', \
prompt for the client to use."
  (interactive "p")
  (setq clocking--session-active-p t)
  (save-restriction
    (widen)
    (let ((clock-at-point (equal arg 4))
          (prompt-for-client (equal arg 16)))
      (cond
       ((and clock-at-point
             (derived-mode-p 'org-agenda-mode)
             (org-with-point-at (org-get-at-bol 'org-hd-marker) (org-get-tags)))
        (org-agenda-clock-in '(16)))
       ((and clock-at-point
             (derived-mode-p 'org-mode)
             (not (org-before-first-heading-p)))
        (org-clock-in '(16)))
       (t
        (clocking--clock-in-on-default prompt-for-client)))))

  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'clocking-agenda-should-update-hook)))

(defun clocking-punch-out ()
  "Stop the clock."
  (interactive)
  (setq clocking--session-active-p nil)
  (when (org-clocking-p)
    (org-clock-out))
  (org-agenda-remove-restriction-lock)
  (org-save-all-org-buffers)
  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'clocking-agenda-should-update-hook))
  (message "Punched out."))

(defun clocking--ancestor-todo-pos ()
  (let (ancestor-todo)
    (while (and (not ancestor-todo) (org-up-heading-safe))
      (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
        (setq ancestor-todo (point))))
    ancestor-todo))

(defun clocking--clock-in-on-parent ()
  (save-excursion
    (save-restriction
      (widen)
      (if-let* ((ancestor-todo (clocking--ancestor-todo-pos)))
          (org-with-point-at ancestor-todo (org-clock-in))
        (clocking--clock-in-on-default)))))

(defun clocking-on-clock-out ()
  (when (and clocking--session-active-p
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (clocking--clock-in-on-parent)))

(provide 'clocking)

;;; clocking.el ends here
