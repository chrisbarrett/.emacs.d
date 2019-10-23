;;; org-project.el --- Project-related orgmode commands  -*- lexical-binding: t; -*-

;;; Commentary:

;; Adapted from:
;;   http://doc.norang.ca/org-mode.html

;;; Code:

(cl-eval-when (compile)
  (require 'org)
  (require 'org-habit)
  (require 'org-agenda))

(defvar org-project-hide-scheduled-and-waiting-next-tasks t)

(defun org-project-find-parent-project ()
  "Move point to the parent (project) task if any."
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun org-project-p ()
  "Any task with a todo keyword subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun org-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (org-project-find-parent-project)
      (if (equal (point) task)
          nil
        t))))

(defun org-project-task-p ()
  "Any task with a todo keyword and no subtask."
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun org-project-is-subproject-p ()
  "Any task which is a subtask of another project."
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun org-project-list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so subtree restrictions list all subtasks.

This is normally used by skipping functions where this variable
is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun org-project-list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so subtree restrictions list all subtasks.

This is normally used by skipping functions where this variable
is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun org-project-toggle-next-task-display ()
  "Toggle display of NEXT and scheduled tasks."
  (interactive)
  (setq org-project-hide-scheduled-and-waiting-next-tasks (not org-project-hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if org-project-hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun org-project-skip-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (org-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun org-project-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (org-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun org-project-skip-non-projects ()
  "Skip trees that are not projects."
  (if (save-excursion (org-project-skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((org-project-p)
            nil)
           ((and (org-project-subtree-p) (not (org-project-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun org-project-skip-project-trees-and-habits ()
  "Skip trees that are projects."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((org-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun org-project-skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and org-project-hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags)))
        next-headline)
       ((org-project-p)
        next-headline)
       ((and (org-project-task-p) (not (org-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun org-project-skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.

When restricted to a project, skip project and sub project tasks,
habits, NEXT tasks, and loose tasks. When not restricted, skip
project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((org-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (org-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (org-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun org-project-skip-project-tasks ()
  "Show non-project tasks.

Skip project and sub-project tasks, habits, and project related
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((org-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((org-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun org-project-skip-non-project-tasks ()
  "Show project tasks.

Skip project and sub-project tasks, habits, and loose non-project
tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (org-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (org-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun org-project-skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits."
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((org-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun org-project-skip-non-subprojects ()
  "Skip trees that are not projects."
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (org-project-is-subproject-p)
        nil
      next-headline)))

(provide 'org-project)

;;; org-project.el ends here
