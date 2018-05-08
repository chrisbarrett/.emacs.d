;;; cb-evil-ispell.el --- Evil configuration for ispell  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett
;; Package-Requires: ((s "1.10.0") (dash "2.12.0"))
;; Author: Chris Barrett <chris+emacs@walrus.cool>

;;; Commentary:

;;; Code:

(require 'flyspell)
(require 'ispell)
(require 's)
(require 'dash)

(autoload 'evil-global-set-key "evil")

(defun cb-evil-ispell--add-to-dict (word)
  "Add WORD to the user's dictionary."
  (ispell-send-string (concat "*" word "\n"))
  (setq ispell-pdict-modified-p '(t))
  (ispell-pdict-save ispell-silently-savep))

(defun cb-evil-ispell-mark-word-as-good (word)
  "Add WORD at point to the Ispell dictionary."
  (interactive (list (thing-at-point 'word)))
  (cb-evil-ispell--add-to-dict word)
  (message "%s added to dictionary" (s-upcase word)))

(defun cb-evil-ispell-correct-word (arg)
  "Corect the word at point with Ispell.
With a number ARG, select the nth replacement."
  (interactive "*P")
  (if (numberp arg)
      (dotimes (_ (1+ arg))
        (flyspell-auto-correct-word))
    (ispell-word)))

(defun cb-evil-ispell-mark-word-as-locally-good (word)
  "Add WORD at point to the list of locally-defined words."
  (interactive (list (thing-at-point 'word)))
  (when word
    (ispell-add-per-file-word-list word)
    (message "%s added to local word list" (s-upcase word))))

(defun cb-evil-ispell--error-backward-search-start-pos (pos)
  "Wrap the search to the end of the buffer if there are no errors before POS."
  (if (and (eq (current-buffer) flyspell-old-buffer-error)
           (eq pos flyspell-old-pos-error))
      (cond
       ((= flyspell-old-pos-error (point-min))
        (message "Restarting from end of buffer")
        (point-max))
       (t
        (save-excursion
          (forward-word -1)
          (point))))
    (point)))

(defun cb-evil-ispell--prev-spelling-error-pos ()
  (let ((pos (cb-evil-ispell--error-backward-search-start-pos (point))))
    (while (and (> pos (point-min))
                (-none? 'flyspell-overlay-p (overlays-at pos)))
      (cl-decf pos))
    pos))

(defun cb-evil-ispell-previous-spelling-error ()
  "Go to the previous flyspell error."
  (interactive)
  (let ((pos (cb-evil-ispell--prev-spelling-error-pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (when (= pos (point-min))
      (message "No more spelling errors"))))

(defun cb-evil-ispell--error-forward-search-start-pos (pos)
  "Wrap the search to the beginning of the buffer if there are no errors forward of POS."
  (if (and (eq (current-buffer) flyspell-old-buffer-error)
           (eq pos flyspell-old-pos-error))
      (cond
       ((= flyspell-old-pos-error (point-max))
        (message "Restarting from beginning of buffer")
        (point-min))
       (t
        (save-excursion
          (forward-word 1)
          (point))))
    (point)))

(defun cb-evil-ispell--next-spelling-error-pos ()
  (let ((pos (cb-evil-ispell--error-forward-search-start-pos (point))))
    (while (and (< pos (point-max))
                (-none? 'flyspell-overlay-p (overlays-at pos)))
      (cl-incf pos))
    pos))

(defun cb-evil-ispell-next-spelling-error ()
  "Go to the next flyspell error."
  (interactive)
  (let ((pos (cb-evil-ispell--next-spelling-error-pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (when (= pos (point-max))
      (message "No more spelling errors"))))

(provide 'cb-evil-ispell)

;;; cb-evil-ispell.el ends here
