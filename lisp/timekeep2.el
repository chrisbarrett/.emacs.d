;;; timekeep2.el --- Clocking workflow based on org-roam-nodes  -*- lexical-binding: t; -*-

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
(require 'org-roam-node)
(require 'org-capture)
(require 'org-clock)
(require 'persist)
(require 'ht)
(require 's)

(defgroup timekeep2 nil
  "Functions for managing client timekeeping with org-clock."
  :group 'productivity
  :prefix "timekeep2-")

(defcustom timekeep2-default-headline-name "Planning & Meetings"
  "The name of the heading to clock in to if not working on a specific task.

The heading will be created if needed."
  :group 'timekeep2
  :type 'string)

(defcustom timekeep2-fallback-work-tag "work"
  "The tag to return from `timekeep2-work-tag' as a fallback.

Will be used if:

- there is no current target, or

- the current target does not have a TIMEKEEP_TAG or CATEGORY
  property."
  :group 'timekeep2
  :type 'string)

(defcustom timekeep2-default-vat-rate nil
  "The rate of VAT/GST charged on invoices to a client.

Should be a floating point number between 0-1, or nil."
  :group 'timekeep2
  :type '(choice float
                 (const :tag "None" nil)))

(defcustom timekeep2-agenda-should-update-hook nil
  "Hook run when a clocking change should update the agenda."
  :group 'timekeep2
  :type 'hook)

(defcustom timekeep2-node-to-name-function #'org-roam-node-title
  "Function taking an `org-roam-node' and returning a company or cilent's name."
  :group 'timekeep2
  :type 'function)


;;; Node properties

(defun timekeep2-nodes ()
  (let ((table (make-hash-table :test #'equal)))
    (dolist (node (org-roam-node-list))
      (when (seq-find (-lambda ((key . _value))
                        (string-prefix-p "TIMEKEEP" key))
                      (org-roam-node-properties node))
        (puthash (org-roam-node-id node) node table)))
    (hash-table-values table)))

(defun timekeep2--node-property (key node)
  (cl-assert (stringp key))
  (cl-assert node)
  (alist-get key (org-roam-node-properties node) nil nil #'equal))

(defun timekeep2-node-code (node)
  "The code associated with NODE, e.g. for use with invoices.

The value is taken from the TIMEKEEP_CODE property."
  (cl-assert node)
  (timekeep2--node-property "TIMEKEEP_CODE" node))

(defun timekeep2-node-tag (node)
  "A tag associated with NODE.

The value is taken from the TIMEKEEP_TAG property, or the
CATEGORY as a fallback."
  (cl-assert node)
  (or (timekeep2--node-property "TIMEKEEP_TAG" node)
      (-some->> (timekeep2--node-property "CATEGORY" node) (downcase))))

(defun timekeep2-node-name (node)
  "The human-readable name of NODE, e.g. a company or client name.

The value is taken from the TIMEKEEP_NAME property. If that
property is not set, it is computed using
`timekeep2-node-to-name-function'."
  (cl-assert node)
  (or (timekeep2--node-property "TIMEKEEP_NAME" node)
      (funcall timekeep2-node-to-name-function node)))

(defun timekeep2-node-vat-rate (node)
  "The VAT/GST rate associated with NODE."
  (cl-assert node)
  (or (timekeep2--node-property "TIMEKEEP_VAT" node)
      timekeep2-default-vat-rate))


;;; UI prompts

(persist-defvar timekeep2--latest-target-id nil
                "The node id of the most recently clocked client or company.")

(defun timekeep2-current-target ()
  (org-roam-node-from-id timekeep2--latest-target-id))

(defun timekeep2-read-target (&optional prompt)
  (let ((table (ht-from-alist (seq-map (lambda (it) (cons (timekeep2-node-name it) it))
                                       (timekeep2-nodes)))))
    (gethash (completing-read (or prompt "Target: ") table nil t)
             table)))

(defun timekeep2-choose-target (&optional prompt)
  (let ((choice (timekeep2-read-target prompt)))
    (setq timekeep2--latest-target-id (org-roam-node-id choice))
    choice))


;;; Clocktree management & clocking integration

(defun timekeep2--clocktree-headline (buffer)
  (cl-assert (buffer-live-p buffer))
  (let ((heading (list timekeep2-default-headline-name
                       (format-time-string "%Y %W"))))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-roam-capture-find-or-create-olp heading)))))

(defun timekeep2--punch-in-for-node (node)
  (cl-assert node)
  (save-window-excursion
    (save-excursion
      (org-roam-node-visit node)
      (org-with-wide-buffer
       (org-with-point-at (timekeep2--clocktree-headline (current-buffer))
         (org-clock-in '(16)))))))

(defvar timekeep2--session-active-p nil)

(defun timekeep2--clock-in-on-default (&optional prompt-for-target-p)
  (timekeep2--punch-in-for-node
   (if prompt-for-target-p
       (timekeep2-choose-target)
     (or (timekeep2-current-target)
         (timekeep2-choose-target)))))

(defun timekeep2--ancestor-todo-pos ()
  (let (ancestor-todo)
    (org-with-wide-buffer
     (while (and (not ancestor-todo) (org-up-heading-safe))
       (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
         (setq ancestor-todo (point)))))
    ancestor-todo))

(defun timekeep2--clock-in-on-parent ()
  (org-with-wide-buffer
   (if-let* ((ancestor-todo (timekeep2--ancestor-todo-pos)))
       (org-with-point-at ancestor-todo (org-clock-in))
     (timekeep2--clock-in-on-default))))

(defun timekeep2--on-clock-in ()
  (let* ((node (org-roam-node-at-point))
         (node-id (org-roam-node-id node)))
    (cond ((seq-contains-p (seq-map #'org-roam-node-id (timekeep2-nodes)) node-id)
           (setq timekeep2--latest-target-id node-id)
           (setq timekeep2--session-active-p t))
          (t
           (user-error "Not in a valid timekeep target node")))))

(defun timekeep2--on-clock-out ()
  (when (and timekeep2--session-active-p
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (timekeep2--clock-in-on-parent)))

(defun timekeep2--heading-function ()
  (let ((headline (substring-no-properties (org-get-heading t t t t))))
    (format "%s/%s"
            (timekeep2-node-name (timekeep2-current-target))
            (org-link-display-format headline))))


;;;###autoload
(define-minor-mode timekeep2-mode
  "Minor mode enabling special handling of org clocks for work timekeeping.

When this mode is active, clocking out behaves differently:

- If clocking out of a nested todo, assume this is a task
  contributing to a larger unit of work. Search up for a parent
  todo to clock in to.

- If there is no parent, clock in to a default task so that time
  is still tracked."
  :group 'timekeep2
  :global t
  (if timekeep2-mode
      (progn
        (setq org-clock-heading-function #'timekeep2--heading-function)
        (add-hook 'org-clock-in-hook #'timekeep2--on-clock-in)
        (add-hook 'org-clock-out-hook #'timekeep2--on-clock-out))
    (setq timekeep2--session-active-p nil)
    (setq org-clock-heading-function nil)
    (remove-hook 'org-clock-in-hook #'timekeep2--on-clock-in)
    (remove-hook 'org-clock-out-hook #'timekeep2--on-clock-out)))

;;;###autoload
(defun timekeep2-work-tag ()
  (or (-some->> (timekeep2-current-target) (timekeep2-node-tag))
      timekeep2-fallback-work-tag))

;;;###autoload
(defun timekeep2-start (&optional arg)
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
    (timekeep2--clock-in-on-default t))
   ((or (equal arg '(16))
        (null org-clock-history)
        (ignore-errors
          ;; Would attempt to clock into the parent of the default heading?
          (org-with-point-at (car org-clock-history)
            (equal timekeep2-default-headline-name (org-get-heading t t t t)))))
    (timekeep2--clock-in-on-default))
   (t
    (condition-case _
        (org-clock-in-last (when (equal arg '(16))
                             '(4)))
      (error (timekeep2--clock-in-on-default)))))

  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'timekeep2-agenda-should-update-hook)))

;;;###autoload
(defun timekeep2-stop ()
  "Clock out, pausing the timekeeping session."
  (interactive)
  (setq timekeep2--session-active-p nil)
  (when (org-clocking-p)
    (org-clock-out))
  (org-agenda-remove-restriction-lock)
  (org-save-all-org-buffers)
  (when (derived-mode-p 'org-agenda-mode)
    ;; Swap agenda due to context change.
    (run-hooks 'timekeep-agenda-should-update-hook))
  (message "Punched out."))

;;;###autoload
(defun timekeep2-visit-node (&optional ask)
  "Open the current timekeep target node.

By default, go to the current target node. With a prefix arg ASK
or if no current target is set, prompt for the node to visit."
  (interactive "P")
  (org-roam-node-visit
   (if (or ask (null timekeep2--latest-target-id))
       (timekeep2-read-target)
     (org-roam-node-from-id timekeep2--latest-target-id))))

;;;###autoload
(defun timekeep2-capture-to-clocktree ()
  "Target-location function for use in capture templates."
  (timekeep2-visit-node)
  (widen)
  (goto-char (timekeep2--clocktree-headline (current-buffer))))

;;;###autoload
(defun timekeep2-capture-to-toplevel ()
  "Target-location function for use in capture templates."
  (timekeep2-visit-node)
  (widen)
  (goto-char (point-max)))

(provide 'timekeep2)

;;; timekeep2.el ends here
