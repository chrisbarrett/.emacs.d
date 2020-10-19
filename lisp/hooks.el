;;; hooks.el --- Extra hooks for my config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar after-theme-change-functions nil
  "Hook functions run after changing theme.

Each hook is called with a symbol, `dark' or `light', depending
on the theme background.")

(defun hooks--call-after-theme-change-functions (&rest _)
  (run-hook-with-args 'after-theme-change-functions
                      (frame-parameter nil 'background-mode)))

(advice-add 'enable-theme :after #'hooks--call-after-theme-change-functions)

(provide 'hooks)

;;; hooks.el ends here
