;;; haskell-pragmas.el --- Utilities for working with Haskell language pragmas.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(defun haskell-pragmas-language-pragmas ()
  (s-split "\n" (s-trim (shell-command-to-string "stack ghc -- --supported-languages"))))

(defun haskell-pragmas-in-buffer-string (s ps)
  (--filter (s-matches? (rx-to-string `(and "{-# LANGUAGE" (+ space) (* nonl) ,it)) s)
            ps))

(defun haskell-pragmas--available-language-pragmas ()
  (let ((ps (haskell-pragmas-language-pragmas)))
    (-difference ps (haskell-pragmas-in-buffer-string (buffer-string) ps))))

(defun haskell-pragmas--goto-buffer-start ()
  (goto-char (point-min))

  ;; Skip #! line
  (when (and (s-matches? (rx bol "#!")
                         (buffer-substring (line-beginning-position) (line-end-position)))
             (search-forward "#!" nil t))
    (goto-char (line-end-position))
    (forward-char 1))

  (while (and (not (eobp))
              (s-blank? (buffer-substring (line-beginning-position) (line-end-position))))
    (forward-line 1)))

;;;###autoload
(defun haskell-pragmas-insert (pragma)
  "Read a language PRAGMA to be inserted at the start of this file."
  (interactive (list (completing-read "Pragma: " (haskell-pragmas--available-language-pragmas) nil t)))
  (let ((s (format "{-# LANGUAGE %s #-}\n" pragma)))
    (save-excursion
      (haskell-pragmas--goto-buffer-start)
      (insert s))))

(provide 'haskell-pragmas)

;;; haskell-pragmas.el ends here
