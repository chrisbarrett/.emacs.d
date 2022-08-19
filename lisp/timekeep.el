;;; timekeep.el --- Clocking workflow based on org-roam-nodes  -*- lexical-binding: t; -*-

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

(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-roam-node)
(require 'persist)

(defgroup timekeep nil
  "Functions for managing client timekeeping with org-clock."
  :group 'productivity
  :prefix "timekeep-")

(defcustom timekeep-default-headline-name "Planning & Meetings"
  "The name of the heading to clock in to if not working on a specific task.

The heading will be created if needed."
  :group 'timekeep
  :type 'string)

(defcustom timekeep-fallback-work-tag "work"
  "The tag to return from `timekeep-work-tag' as a fallback.

Will be used if:

- there is no current target, or

- the current target does not have a TIMEKEEP_TAG or CATEGORY
  property."
  :group 'timekeep
  :type 'string)

(defcustom timekeep-agenda-should-update-hook nil
  "Hook run when a clocking change should update the agenda."
  :group 'timekeep
  :type 'hook)

(defcustom timekeep-node-to-name-function #'org-roam-node-title
  "Function taking an `org-roam-node' and returning a company or cilent's name."
  :group 'timekeep
  :type 'function)


;;; Node properties

(defun timekeep-nodes ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (org-roam-node-list))
      (when (seq-find (-lambda ((key . _value))
                        (string-prefix-p "TIMEKEEP" key))
                      (org-roam-node-properties node))
        (puthash (org-roam-node-id node) node table)))
    (hash-table-values table)))

(defun timekeep--node-property (key node)
  (cl-assert (stringp key))
  (cl-assert node)
  (alist-get key (org-roam-node-properties node) nil nil #'equal))

(defun timekeep-node-code (node &optional assert)
  "The code associated with NODE, e.g. for use with invoices.

The value is taken from the TIMEKEEP_CODE property.

If ASSERT is non-nil, throw an error on a nil result."
  (cl-assert node)
  (let ((result (timekeep--node-property "TIMEKEEP_CODE" node)))
    (when assert
      (cl-assert result))
    result))

(defun timekeep-node-tag (node &optional assert)
  "A tag associated with NODE.

The value is taken from the TIMEKEEP_TAG property, or the
CATEGORY as a fallback.

If ASSERT is non-nil, throw an error on a nil result."
  (cl-assert node)
  (let ((result (or (timekeep--node-property "TIMEKEEP_TAG" node)
                    (-some->> (timekeep--node-property "CATEGORY" node) (downcase)))))
    (when assert
      (cl-assert result))
    result))

(defun timekeep-node-name (node)
  "The human-readable name of NODE, e.g. a company or client name.

The value is taken from the TIMEKEEP_NAME property. If that
property is not set, it is computed using
`timekeep-node-to-name-function'."
  (cl-assert node)
  (or (timekeep--node-property "TIMEKEEP_NAME" node)
      (funcall timekeep-node-to-name-function node)))


;;; UI prompts

(persist-defvar timekeep--latest-target-id nil
                "The node id of the most recently clocked client or company.")

(defun timekeep-current-target ()
  (org-roam-node-from-id timekeep--latest-target-id))

(defun timekeep-read-target ()
  (let* ((alist (seq-map (lambda (it) (cons (timekeep-node-name it) it))
                         (timekeep-nodes)))
         (choice (completing-read "Target: " alist nil t)))
    (alist-get choice alist nil nil #'equal)))

(defun timekeep-choose-target (&optional interactive-p)
  "Choose a target node for clocking with timekeep.

With optional argument INTERACTIVE-P, log additional messages as
feedback."
  (interactive (list t))
  (let ((node (timekeep-read-target)))
    (setq timekeep--latest-target-id (org-roam-node-id node))
    (persist-save 'timekeep--latest-target-id)
    (when interactive-p
      (message (concat "Timekeep traget set to " (propertize (timekeep-node-name node)
                                                             'face
                                                             'font-lock-string-face))))
    node))


;;; Clocktree management & clocking integration

(defun timekeep--clocktree-headline-find-or-create ()
  (let ((heading (list timekeep-default-headline-name (format-time-string "%Y %W"))))
    (widen)
    (goto-char (marker-position (org-roam-capture-find-or-create-olp heading)))))

(defun timekeep--punch-in-for-node (node)
  (cl-assert node)
  (save-window-excursion
    (save-excursion
      (org-roam-node-visit node)
      (org-with-wide-buffer
       (timekeep--clocktree-headline-find-or-create)
       (org-clock-in '(16))))))

(defvar timekeep--session-active-p nil)

(defun timekeep--clock-in-on-default (&optional prompt-for-target-p)
  (timekeep--punch-in-for-node
   (if prompt-for-target-p
       (timekeep-choose-target)
     (or (timekeep-current-target)
         (timekeep-choose-target)))))

(defun timekeep--ancestor-todo-pos ()
  (let (ancestor-todo)
    (org-with-wide-buffer
     (while (and (not ancestor-todo) (org-up-heading-safe))
       (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
         (setq ancestor-todo (point)))))
    ancestor-todo))

(defun timekeep--clock-in-on-parent ()
  (org-with-wide-buffer
   (if-let* ((ancestor-todo (timekeep--ancestor-todo-pos)))
       (org-with-point-at ancestor-todo (org-clock-in))
     (timekeep--clock-in-on-default))))

(defun timekeep--on-clock-in ()
  (let* ((node (org-roam-node-at-point))
         (node-id (org-roam-node-id node)))
    (cond ((seq-contains-p (seq-map #'org-roam-node-id (timekeep-nodes)) node-id)
           (setq timekeep--latest-target-id node-id)
           (persist-save 'timekeep--latest-target-id)
           (setq timekeep--session-active-p t))
          (t
           (user-error "Not in a valid timekeep target node")))))

(defun timekeep--on-clock-out ()
  (when (and timekeep--session-active-p
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (timekeep--clock-in-on-parent)))

(defun timekeep--heading-function ()
  (let ((headline (substring-no-properties (org-get-heading t t t t))))
    (format "%s/%s"
            (timekeep-node-name (timekeep-current-target))
            (org-link-display-format headline))))


;;;###autoload
(define-minor-mode timekeep-mode
  "Minor mode enabling special handling of org clocks for work timekeeping.

When this mode is active, clocking out behaves differently:

- If clocking out of a nested todo, assume this is a task
  contributing to a larger unit of work. Search up for a parent
  todo to clock in to.

- If there is no parent, clock in to a default task so that time
  is still tracked."
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
(defun timekeep-work-tag ()
  "Return the org tag associated with the current timekeep target.

If the target does not have one the value of
`timekeep-fallback-work-tag' is used."
  (or (-some->> (timekeep-current-target) (timekeep-node-tag))
      timekeep-fallback-work-tag))

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
   ((equal arg '(64))
    (timekeep--clock-in-on-default t))
   ((or (equal arg '(16))
        (null org-clock-history)
        (ignore-errors
          ;; Would attempt to clock into the parent of the default heading?
          (org-with-point-at (car org-clock-history)
            (equal timekeep-default-headline-name (org-get-heading t t t t)))))
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
(defun timekeep-visit-node (&optional ask)
  "Open the current timekeep target node.

By default, go to the current target node. With a prefix arg ASK
or if no current target is set, prompt for the node to visit."
  (interactive "P")
  (org-roam-node-visit
   (if (or ask (null timekeep--latest-target-id))
       (timekeep-choose-target t)
     (org-roam-node-from-id timekeep--latest-target-id))))

;;;###autoload
(defun timekeep-capture-to-clocktree ()
  "Target-location function for use in capture templates."
  (timekeep-visit-node)
  (widen)
  (timekeep--clocktree-headline-find-or-create))

;;;###autoload
(defun timekeep-capture-to-toplevel ()
  "Target-location function for use in capture templates."
  (timekeep-visit-node)
  (widen)
  (goto-char (point-max)))

(provide 'timekeep)

;;; timekeep.el ends here
