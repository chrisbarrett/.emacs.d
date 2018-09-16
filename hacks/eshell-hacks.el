;;; eshell-hacks.el --- Hacks for eshell  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'el-patch)

(el-patch-feature 'eshell)

(with-eval-after-load 'em-hist
  (with-no-warnings

    ;; Make eshell history management strip text properties. This is needed to
    ;; so that eshell's history commands work after submitted commands have the
    ;; read-only text property applied in my config.

    (el-patch-defun eshell-add-command-to-history ()
      "Add the command entered at `eshell-command's prompt to the history ring.
The command is added to the input history ring, if the value of
variable `eshell-input-filter' returns non-nil when called on the
command.

This function is supposed to be called from the minibuffer, presumably
as a minibuffer-exit-hook."
      (eshell-add-input-to-history
       ((el-patch-swap buffer-substring buffer-substring-no-properties)
        (minibuffer-prompt-end) (point-max))))

    (el-patch-defun eshell-add-to-history ()
      "Add last Eshell command to the history ring.
The command is entered into the input history ring, if the value of
variable `eshell-input-filter' returns non-nil when called on the
command."
      (when (> (1- eshell-last-input-end) eshell-last-input-start)
        (let ((input ((el-patch-swap buffer-substring buffer-substring-no-properties)
                      eshell-last-input-start
                      (1- eshell-last-input-end))))
          (eshell-add-input-to-history input))))

    (el-patch-defun eshell-previous-matching-input (regexp arg)
      "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
      (interactive (eshell-regexp-arg "Previous input matching (regexp): "))
      (setq arg (eshell-search-arg arg))
      (if (> eshell-last-output-end (point))
          (error "Point not located after prompt"))
      (let ((pos (eshell-previous-matching-input-string-position regexp arg)))
        ;; Has a match been found?
        (if (null pos)
            (error "Not found")
          (setq eshell-history-index pos)
          (unless (minibuffer-window-active-p (selected-window))
            (message "History item: %d" (- (ring-length eshell-history-ring) pos)))
          ;; Can't use kill-region as it sets this-command
          (delete-region eshell-last-output-end (point))
          (insert-and-inherit (el-patch-swap
                                (eshell-get-history pos)
                                (substring-no-properties
                                 (eshell-get-history pos)))))))))

(provide 'eshell-hacks)

;;; eshell-hacks.el ends here
