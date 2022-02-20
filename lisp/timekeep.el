;;; timekeep.el --- Functions for managing clocking and timekeeping  -*- lexical-binding: t; -*-

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

;; Minor mode enabling special handling of org clocks for client timekeeping.

;; This mode allows you to set up a selection of org-roam nodes that
;; are considered independent projects for timekeeping (in
;; particular, clients for consulting work).

;; When this mode is active, clocking out behaves differently:

;; - If clocking out of a nested todo, assume this is a task
;;   contributing to a larger unit of work. Search up for a parent
;;   todo to clock in to.

;; - If there is no parent, clock in to a default task so that time
;;   is still tracked.

;; Customize `timekeep-clients' to define which roam nodes are
;; used.

;; Adapted from http://doc.norang.ca/org-mode.html#Clocking

;;; Code:

(require 'ht)
(require 'subr-x)

(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-roam)

(defgroup timekeep nil
  "Functions for managing client timekeeping with org-clock."
  :group 'productivity
  :prefix "timekeep-")

(defcustom timekeep-default-headline-name "Planning & Meetings"
  "The name of the heading to clock in to if not working on a specific task.

The heading will be created if needed."
  :group 'timekeep
  :type 'string)

(defcustom timekeep-agenda-should-update-hook nil
  "Hook run when a clocking change should update the agenda."
  :group 'timekeep
  :type 'hook)

(defcustom timekeep-cache-file (locate-user-emacs-file "timekeep")
  "Path to timekeep's cache file.

This is used to persist the selected client between Emacs
sessions."
  :group 'timekeep
  :type 'file)


;; Client specification

(defcustom timekeep-clients nil
  "List of clients with their associated data for invoicing.

Each value is a plist with at lesat the following keys:

:name (required) - A human-readable name for the client (used in prompts).

:roam-id (required) - The ID is the org-roam node for the client's notes file."
  :group 'timekeep
  :type '(alist :key-type string :value-type (plist :value-type string)))

(defun timekeep-find (attr value)
  (seq-find (lambda (it) (equal (plist-get it attr) value))
            timekeep-clients))

(defun timekeep-list (attr)
  (seq-map (lambda (it) (plist-get it attr))
           timekeep-clients))


;; Choosing client specification by name

(defvar timekeep--last-client-name nil)

(defun timekeep--last-client-name ()
  (unless timekeep--last-client-name
    (ignore-errors
      (setq timekeep--last-client-name (f-read-text timekeep-cache-file 'utf-8))))
  timekeep--last-client-name)

(defun timekeep--set-latest-client-name (client-name)
  (setq timekeep--last-client-name client-name)
  (f-write-text client-name 'utf-8 timekeep-cache-file))

(defun timekeep-read-client-name ()
  (let ((choice (completing-read "Client: " (timekeep-list :name) nil t nil 'timekeep--choose-buffer-history
                                 (timekeep--last-client-name))))
    (timekeep--set-latest-client-name choice)
    choice))

(defun timekeep-choose-client ()
  (timekeep-find :name (timekeep-read-client-name)))


;; Org-roam file & node lookup for clients

(defvar timekeep--client-nodes nil)

(defun timekeep--client-nodes ()
  (when (or (null timekeep--client-nodes)
            (hash-table-empty-p timekeep--client-nodes))
    (let* ((ids (timekeep-list :roam-id))
           (nodes (ht-values (seq-reduce (lambda (acc it)
                                           (let ((id (org-roam-node-id it)))
                                             (when (member id ids)
                                               (ht-set acc id it)))
                                           acc)
                                         (org-roam-node-list)
                                         (ht-create)))))
      (setq timekeep--client-nodes (ht-from-alist (seq-map (lambda (node)
                                                             (cons (org-roam-node-title node) node))
                                                           nodes)))))
  timekeep--client-nodes)

(defun timekeep-client-node-files ()
  (seq-map #'org-roam-node-file (ht-values (timekeep--client-nodes))))

(defun timekeep-find-roam-node-by-name (name)
  (ht-get (timekeep--client-nodes) name))

(defun timekeep-last-roam-node (&optional ask)
  (let ((client (timekeep--last-client-name)))
    (timekeep-find-roam-node-by-name (if (or ask (null client))
                                         (timekeep-read-client-name)
                                       client))))


;; Clocking

(defun timekeep--clocktree-headline (buffer)
  (let ((heading (list timekeep-default-headline-name
                       (format-time-string "%Y %W"))))
    (with-current-buffer buffer
      (org-roam-capture-find-or-create-olp heading))))

(defun timekeep--punch-in-for-node (node)
  (save-window-excursion
    (save-excursion
      (org-roam-node-visit node)
      (org-with-point-at (timekeep--clocktree-headline (current-buffer))
        (org-clock-in '(16))))))

(defvar timekeep--session-active-p nil)

(defun timekeep--clock-in-on-default (&optional ask)
  (timekeep--punch-in-for-node (timekeep-last-roam-node ask)))

(defun timekeep--ancestor-todo-pos ()
  (let (ancestor-todo)
    (while (and (not ancestor-todo) (org-up-heading-safe))
      (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
        (setq ancestor-todo (point))))
    ancestor-todo))

(defun timekeep--clock-in-on-parent ()
  (org-with-wide-buffer
   (if-let* ((ancestor-todo (timekeep--ancestor-todo-pos)))
       (org-with-point-at ancestor-todo (org-clock-in))
     (timekeep--clock-in-on-default))))

(defun timekeep--on-clock-in ()
  (-when-let* ((id (org-roam-id-at-point))
               ((&plist :name) (timekeep-find :roam-id id)))
    (timekeep--set-latest-client-name name)
    (setq timekeep--session-active-p t)))

(defun timekeep--on-clock-out ()
  (when (and timekeep--session-active-p
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (timekeep--clock-in-on-parent)))



(defun timekeep--heading-function ()
  (let ((headline (substring-no-properties (org-get-heading t t t t))))
    (format "%s/%s"
            (timekeep--last-client-name)
            (org-link-display-format headline))))

;;;###autoload
(define-minor-mode timekeep-mode
  "Minor mode enabling special handling of org clocks for client timekeeping.

This mode allows you to set up a selection of org-roam nodes that
are considered independent projects for timekeeping (in
particular, clients for consulting work).

When this mode is active, clocking out behaves differently:

- If clocking out of a nested todo, assume this is a task
  contributing to a larger unit of work. Search up for a parent
  todo to clock in to.

- If there is no parent, clock in to a default task so that time
  is still tracked.

Customize `timekeep-clients' to define which roam nodes are
used."
  :group 'timekeep
  :global t
  (if timekeep-mode
      (progn
        (setq org-clock-heading-function #'timekeep--heading-function)
        (add-hook 'org-clock-in-hook #'timekeep--on-clock-in)
        (add-hook 'org-clock-out-hook #'timekeep--on-clock-out))
    (setq timekeep--session-active-p nil)
    (setq org-clock-heading-function nil)
    (remove-hook 'org-clock-in-hook #'timekeep--on-clock-in)
    (remove-hook 'org-clock-out-hook #'timekeep--on-clock-out)))

;;;###autoload
(defun timekeep-start (&optional arg)
  "Start a timekeeping session.

The previous clock is resumed by default.

With single prefix ARG, or if there is no previous clock, clock
in on the default headline for the current client.

With two prefix args, select from a list of recently clocked
tasks.

With three prefix args, prompt for the client to use and clock in
on the default headline for that client."
  (interactive "P")
  (cond
   ((equal arg '(32))
    (timekeep--clock-in-on-default t))
   ((or (equal arg '(4))
        (null org-clock-history)
        ;; Would attempt to clock into the parent of the default heading?
        (org-with-point-at (car org-clock-history)
          (equal timekeep-default-headline-name (org-get-heading t t t t))))
    (timekeep--clock-in-on-default))
   (t
    (condition-case _
        (org-clock-in-last (when (equal arg '(16))
                             '(4)))
      (error (timekeep--clock-in-on-default)))))

  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'timekeep-agenda-should-update-hook)))

;;;###autoload
(defun timekeep-stop ()
  "Clock out, pausing the timekeeping session."
  (interactive)
  (setq timekeep--session-active-p nil)
  (when (org-clocking-p)
    (org-clock-out))
  (org-agenda-remove-restriction-lock)
  (org-save-all-org-buffers)
  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'timekeep-agenda-should-update-hook))
  (message "Punched out."))

;;;###autoload
(defun timekeep-capture-target ()
  (org-roam-node-visit (timekeep-last-roam-node))
  (widen)
  (goto-char (point-max)))

;;;###autoload
(defun timekeep-meeting-capture-target ()
  (org-roam-node-visit (timekeep-last-roam-node))
  (widen)
  (goto-char (timekeep--clocktree-headline (current-buffer))))

;;;###autoload
(defun timekeep-work-tag ()
  (let* ((id (org-roam-node-id (timekeep-last-roam-node)))
         (client (timekeep-find :roam-id id)))
    (plist-get client :tag)))

;;;###autoload
(defun timekeep-find-client-buffer (&optional ask)
  "Open the org-roam buffer for the current client.

With prefix arg ASK, prompt for the client to open."
  (interactive "P")
  (org-roam-node-visit (timekeep-last-roam-node ask)))

(provide 'timekeep)

;;; timekeep.el ends here
