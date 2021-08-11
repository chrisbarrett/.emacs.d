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

;; Customize `timekeep-clients-alist' to define which roam nodes are
;; used.
;;
;; Adapted from http://doc.norang.ca/org-mode.html#Clocking

;;; Code:

(require 'ht)
(require 'subr-x)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-agenda)
  (require 'org-clock)
  (require 'org-roam))

(defgroup timekeep nil
  "Functions for managing client timekeeping with org-clock."
  :group 'productivity
  :prefix "timekeep-")

(defcustom timekeep-clients-alist nil
  "Alist mapping org tags to IDs.

Tags are expected to be file tags, applied to todos for that
client. The ID is the org-roam node for the client's notes file."
  :group 'timekeep
  :type '(alist :key-type string :value-type string))

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
  "Path to timekeep's cache file."
  :group 'timekeep
  :type 'file)



(defun timekeep--ensure-default-headline (buffer)
  "Create the default heading for clocking in BUFFER.

Return the position of the headline."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (if-let* ((marker (org-find-exact-headline-in-buffer timekeep-default-headline-name)))
            (marker-position marker)
          (goto-char (point-max))
          (delete-horizontal-space)
          (org-insert-heading nil nil t)
          (insert timekeep-default-headline-name)
          (point))))))

(defun timekeep--client-node-ids ()
  (seq-map 'cdr timekeep-clients-alist))

(defun timekeep--choose-tag-for-node-id (id)
  (or (car (seq-find (lambda (it) (equal (cdr it) id))
                     timekeep-clients-alist))
      (completing-read "Tag:" (seq-map 'car timekeep-clients-alist))))

(defvar timekeep--last-client-choice nil
  "The title of the last client selected by `timekeep--choose-client-node'.")

(defun timekeep--last-client-choice ()
  (unless timekeep--last-client-choice
    (ignore-errors
      (setq timekeep--last-client-choice (f-read-text timekeep-cache-file 'utf-8))))
  timekeep--last-client-choice)

(defun timekeep--set-last-client-choice (node-title)
  (setq timekeep--last-client-choice node-title)
  (f-write-text node-title 'utf-8 timekeep-cache-file))

(defvar timekeep--client-nodes-cache nil
  "Hash-table of titles to org-roam-nodes.")

(defun timekeep--client-nodes ()
  (when (or (null timekeep--client-nodes-cache)
            (hash-table-empty-p timekeep--client-nodes-cache))
    (let ((nodes (ht-values (seq-reduce (lambda (acc it)
                                          (let ((id (org-roam-node-id it)))
                                            (when (member id (timekeep--client-node-ids))
                                              (ht-set acc id it)))
                                          acc)
                                        (org-roam-node-list)
                                        (ht-create)))))
      (setq timekeep--client-nodes-cache (ht-from-alist (seq-map (lambda (node)
                                                                   (cons (org-roam-node-title node) node))
                                                                 nodes)))))
  timekeep--client-nodes-cache)

(defun timekeep--heading-function ()
  (let ((headline (substring-no-properties (org-get-heading t t t t))))
    (format "%s/%s"
            (timekeep--last-client-choice)
            (with-temp-buffer
              (insert headline)
              (org-mode)
              (font-lock-ensure)
              (buffer-string)))))

(defun timekeep--choose-client-node ()
  (let* ((nodes (timekeep--client-nodes))
         (choice (completing-read "Client: " (ht-keys nodes) nil t nil 'timekeep--choose-buffer-history
                                  (timekeep--last-client-choice))))
    (timekeep--set-last-client-choice choice)
    (ht-get nodes choice)))

(defun timekeep--get-node-by-name (name)
  (ht-get (timekeep--client-nodes) name))

(defun timekeep--punch-in-for-node (node)
  (with-current-buffer (org-roam-node--find node)
    (org-with-point-at (timekeep--ensure-default-headline (current-buffer))
      (org-clock-in '(16)))))

(defvar timekeep--session-active-p nil)

(defun timekeep--clock-in-on-default (&optional prompt-for-client)
  (timekeep--punch-in-for-node
   (if prompt-for-client
       (timekeep--choose-client-node)
     (timekeep--get-node-by-name (timekeep--last-client-choice)))))

(defun timekeep--ancestor-todo-pos ()
  (let (ancestor-todo)
    (while (and (not ancestor-todo) (org-up-heading-safe))
      (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
        (setq ancestor-todo (point))))
    ancestor-todo))

(defun timekeep--clock-in-on-parent ()
  (save-excursion
    (save-restriction
      (widen)
      (if-let* ((ancestor-todo (timekeep--ancestor-todo-pos)))
          (org-with-point-at ancestor-todo (org-clock-in))
        (timekeep--clock-in-on-default)))))

(defun timekeep--on-clock-in ()
  (let ((files (ht-map (lambda (_key node)
                         (cons (org-roam-node-file node)
                               node))
                       (timekeep--client-nodes))))
    (when-let* ((node (alist-get (buffer-file-name) files nil nil #'equal)))
      (setq timekeep--last-client-choice (org-roam-node-title node))
      (setq timekeep--session-active-p t))))

(defun timekeep--on-clock-out ()
  (when (and timekeep--session-active-p
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (timekeep--clock-in-on-parent)))



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

Customize `timekeep-clients-alist' to define which roam nodes are
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
(defun timekeep-punch-in (&optional arg)
  "Punch in with the default date tree for a client.

Remembers the last client chosen.

With a `\\[universal-argument]' prefix ARG, prompt for the client
to use."
  (interactive "P")
  (save-restriction
    (widen)
    (timekeep--clock-in-on-default arg))

  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'timekeep-agenda-should-update-hook)))

;;;###autoload
(defun timekeep-punch-out ()
  "Stop the clock."
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
  (org-roam-node--find
   (if (null timekeep--last-client-choice)
       (timekeep--choose-client-node)
     (timekeep--get-node-by-name timekeep--last-client-choice)))
  (widen)
  (goto-char (point-max)))

;;;###autoload
(defun timekeep-work-tag ()
  (let ((node
         (if timekeep--last-client-choice
             (timekeep--get-node-by-name timekeep--last-client-choice)
           (timekeep--choose-client-node))))
    (timekeep--choose-tag-for-node-id (org-roam-node-id node))))

;;;###autoload
(defun timekeep-client-files ()
  (seq-map #'org-roam-node-file (ht-values (timekeep--client-nodes))))

;;;###autoload
(defun timekeep-find-client-buffer (&optional arg)
  "Open the org-roam buffer for the current client.

With prefix arg ARG, prompt for the client to open."
  (interactive "P")
  (switch-to-buffer
   (org-roam-node--find
    (cond
     (arg
      (timekeep--choose-client-node))
     (timekeep--last-client-choice
      (timekeep--get-node-by-name timekeep--last-client-choice))
     (t
      (timekeep--choose-client-node))))))

(provide 'timekeep)

;;; timekeep.el ends here
