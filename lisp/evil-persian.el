;;; evil-persian.el --- Evil state for typing Persian characters.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'evil)
(require 'dash)

(evil-define-state persian-insert
  "Insert state, where keys are remapped to Persian characters"
  :tag " <I>[ف] "
  :cursor (bar . 2)
  :message "-- INSERT (PERSIAN) --"
  :entry-hook (evil-start-track-last-insertion)
  :exit-hook (evil-cleanup-insert-state evil-stop-track-last-insertion)
  :input-method t
  (cond
   ((evil-persian-insert-state-p)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (setq evil-maybe-remove-spaces t)
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step)))
   (t
    (remove-hook 'post-command-hook #'evil-maybe-remove-spaces)
    (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
    (evil-maybe-remove-spaces t)
    (setq evil-insert-repeat-info evil-repeat-info)
    (evil-set-marker ?^ nil t)
    (unless (eq evil-want-fine-undo t)
      (evil-end-undo-step))
    (when evil-move-cursor-back
      (when (or (evil-normal-state-p evil-next-state)
                (evil-motion-state-p evil-next-state))
        (evil-move-cursor-back))))))

(define-key evil-persian-insert-state-map (kbd "<escape>") #'evil-normal-state)

(-each '(
         ;; Number row
         ("$" . "‍") ("~" . "÷")
         ("&" . "۱") ("%" . "!")
         ("[" . "۲") ("7" . "٬")
         ("{" . "۳") ("5" . "٫")
         ("}" . "۴") ("3" . "﷼")
         ("(" . "۵") ("1" . "٪")
         ("=" . "۶") ("9" . "×")
         ("*" . "۷") ("0" . "،")
         (")" . "۸") ("2" . "*")
         ("+" . "۹") ("4" . ")")
         ("]" . "۰") ("6" . "(")
         ("!" . "-") ("8" . "ـ")
         ("#" . "=") ("`" . "+")

         (":" . "ض") (";" . "ْ")
         ("," . "ص") ("<" . "ٌ")
         ("." . "ث") (">" . "ٍ")
         ("p" . "ق") ("P" . "ً")
         ("y" . "ف") ("Y" . "ُ")
         ("f" . "غ") ("F" . "ِ")
         ("g" . "ع") ("G" . "َ")
         ("c" . "ه") ("C" . "ّ")
         ("r" . "خ") ("R" . "]")
         ("l" . "ح") ("L" . "[")
         ("/" . "ج") ("?" . "}")
         ("@" . "چ") ("^" . "{")
         ("|" . "\\") ("\\" . "|")

         ("a" . "ش") ("A" . "ؤ")
         ("o" . "س") ("O" . "ئ")
         ("e" . "ی") ("E" . "ي")
         ("u" . "ب") ("U" . "إ")
         ("i" . "ل") ("I" . "أ")
         ("d" . "ا") ("D" . "آ")
         ("h" . "ت") ("H" . "ة")
         ("t" . "ن") ("T" . "«")
         ("n" . "م") ("N" . "»")
         ("s" . "ک") ("S" . ":")
         ("-" . "گ") ("_" . "؛")

         ("'" . "ظ") ("\"" . "ك")
         ("q" . "ط") ("Q" . "ٓ")
         ("j" . "ز") ("J" . "ژ")
         ("k" . "ر") ("K" . "ٰ")
         ("x" . "ذ") ("X" . "‌")
         ("b" . "د") ("B" . "ٔ")
         ("m" . "پ") ("M" . "ء")
         ("w" . "و") ("W" . "<")
         ("v" . ".") ("V" . ">")
         ("z" . "/") ("Z" . "؟"))
  (-lambda ((k . v))
    (define-key evil-persian-insert-state-map (kbd k) (lambda ()
                                            (interactive)
                                            (insert v)))))

(define-minor-mode evil-persian-mode
  "Minor mode substituting insert state for persian insert state."
  nil nil nil

  (setq-local evil-normal-state-tag (if evil-persian-mode " <N>[ف] " " <N> "))
  (setq-local evil-emacs-state-tag (if evil-persian-mode " <E>[ف] " " <E> "))
  (setq-local evil-motion-state-tag (if evil-persian-mode " <M>[ف] " " <M> "))
  (setq-local evil-replace-state-tag (if evil-persian-mode " <R>[ف] " " <R> "))
  (setq-local evil-visual-state-tag (if evil-persian-mode " <V>[ف] " " <V> "))

  ;; Switch between Persian and Latin chars.
  (when (or (evil-persian-insert-state-p)
            (evil-insert-state-p))
    ;; NOTE: assume advice is installed on `evil-insert' so we enter the correct
    ;; state.
    (evil-insert nil)))

(defun evil-persian--insert-hack-enable (f &optional arg)
  (if evil-persian-mode
      (evil-persian-insert-state arg)
    (funcall f arg)))

(advice-add 'evil-insert-state :around #'evil-persian--insert-hack-enable)

(provide 'evil-persian)

;;; evil-persian.el ends here
