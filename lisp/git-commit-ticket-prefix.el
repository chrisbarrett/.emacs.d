;;; git-commit-ticket-prefix.el --- Prefix commit messages with ticket numbers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

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

;; A helper library that automatically prepends a ticket number to your commit
;; messages in Magit based on the branch name.

;; It extracts the issue ticket number from the current branch name, and prepends
;; the ticket number to each commit messages.

;;; Code:

(require 's)
(require 'git-commit)

(defvar git-commit-ticket-prefix-format-string "%s: ")

(autoload 'magit-get-current-branch "magit-git")

(defun git-commit-ticket-prefix--ticket-number ()
  (-when-let* ((branch (magit-get-current-branch))
               ((_ ticket) (s-match (rx bos (? (or "feature" "release") "/")
                                        (group (+ alpha) (? "-") (+ digit))
                                        "-") branch)))
    ticket))

(defun git-commit-ticket-prefix--message-contains-ticket-number? (ticket-number)
  (save-excursion
    (goto-char (point-min))
    (s-contains? ticket-number (buffer-substring (line-beginning-position)
                                                 (line-end-position)))))

;;;###autoload
(defun git-commit-ticket-prefix-insert-ticket-number (buffer)
  "Insert the ticket number, unless it's already in BUFFER."
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (-when-let (ticket (git-commit-ticket-prefix--ticket-number))
      (unless (git-commit-ticket-prefix--message-contains-ticket-number? ticket)
        (goto-char (point-min))
        (goto-char (line-beginning-position))
        (insert (format git-commit-ticket-prefix-format-string ticket))
        (just-one-space)
        t))))

;;;###autoload
(defun git-commit-ticket-prefix-insert ()
  (let ((buf (current-buffer)))
    ;; Run after `server-execute', which is run using
    ;; a timer which starts immediately.
    (run-with-timer 0.01 nil
                    (lambda ()
                      (when (git-commit-ticket-prefix-insert-ticket-number buf)
                        (run-with-timer 0.1 nil
                                        (lambda ()
                                          (goto-char (line-end-position)))))))))

;;;###autoload
(define-minor-mode git-commit-ticket-prefix-autoinsert-mode
  "Minor mode enabling automatic insertion of ticket prefixes to commit messages."
  :group 'git-commit-ticket-prefix
  :global t
  (if git-commit-ticket-prefix-autoinsert-mode
      (add-hook 'git-commit-setup-hook #'git-commit-ticket-prefix-insert)
    (remove-hook 'git-commit-setup-hook #'git-commit-ticket-prefix-insert)))

(provide 'git-commit-ticket-prefix)

;;; git-commit-ticket-prefix.el ends here
