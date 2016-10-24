;;; cb-ivy-occur-then-wgrep.el --- Command to open wgrep buffer from ivy result.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'noflet)

(autoload 'ivy-occur "ivy")
(autoload 'ivy-wgrep-change-to-wgrep-mode "ivy")

(defun cb-ivy-occur-then-wgrep ()
  "Shortcut for calling `ivy-occur' then activating wgrep."
  (interactive)
  (noflet
    ;; HACK: Run the original exit callback, then assume the occur buffer is
    ;; being displayed and change to wgrep.
    ((ivy-exit-with-action
      (action)
      (funcall this-fn (lambda (&rest args)
                         (apply action args)
                         (ivy-wgrep-change-to-wgrep-mode)))))
    (ivy-occur)))

(provide 'cb-ivy-occur-then-wgrep)

;;; cb-ivy-occur-then-wgrep.el ends here
