;;; hide-header-line.el --- minor mode that hides/masks your header line -*- lexical-binding: t; -*-
;;; Commentary:

;; Shamelessly adapted from `hide-mode-line-mode' by Henrik Lissner.

;;; Code:

(defvar hide-header-line-format nil
  "The modeline format to use when `hide-header-line-mode' is active.")

(defvar hide-header-line-excluded-modes '(fundamental-mode)
  "List of major modes where `global-hide-header-line-mode' won't affect.")

(defvar-local hide-header-line--old-format nil
  "Storage for the old `header-line-format', so it can be restored when
`hide-header-line-mode' is disabled.")

;;;###autoload
(define-minor-mode hide-header-line-mode
  "Minor mode to hide the header-line in the current buffer."
  :init-value nil
  :global nil
  (if hide-header-line-mode
      (progn
        (add-hook 'after-change-major-mode-hook #'hide-header-line-reset nil t)
        (setq hide-header-line--old-format header-line-format
              header-line-format hide-header-line-format))
    (remove-hook 'after-change-major-mode-hook #'hide-header-line-reset t)
    (setq header-line-format hide-header-line--old-format
          hide-header-line--old-format nil))
  (force-mode-line-update))

;; Ensure major-mode or theme changes don't overwrite these variables
(put 'hide-header-line--old-format 'permanent-local t)
(put 'hide-header-line-mode 'permanent-local-hook t)
(put 'hide-header-line-reset 'permanent-local-hook t)

(defun hide-header-line-reset ()
  "Reset `hide-header-line-mode' in the current buffer, if necessary.

Sometimes, a major mode is activated after `hide-header-line-mode' is activated,
thus disabling it (because changing major modes invokes
`kill-all-local-variables' and specifically kills `header-line-format's local
value, whether or not it's permanent-local.

Attach this to `after-change-major-mode-hook' and `hide-header-line-mode' will be
cycled to fix this."
  (when hide-header-line-mode
    (hide-header-line-mode -1)
    (hide-header-line-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-hide-header-line-mode
  hide-header-line-mode turn-on-hide-header-line-mode)

;;;###autoload
(defun turn-on-hide-header-line-mode ()
  "Turn on `hide-header-line-mode'.
Unless in `fundamental-mode' or `hide-header-line-excluded-modes'."
  (unless (memq major-mode hide-header-line-excluded-modes)
    (hide-header-line-mode +1)))

;;;###autoload
(defun turn-off-hide-header-line-mode ()
  "Turn off `hide-header-line-mode'."
  (hide-header-line-mode -1))

(provide 'hide-header-line)

;;; hide-header-line.el ends here
