;;; evil-bidi.el --- Better support for RTL languages with evil.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'evil)

(evil-define-motion evil-bidi-backward-char (count &optional crosslines noerror)
  "Bidi-aware evil motion."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (pcase (current-bidi-paragraph-direction)
    (`left-to-right
     (evil-backward-char count crosslines noerror))
    (`right-to-left
     (evil-forward-char count crosslines noerror))))

(evil-define-motion evil-bidi-forward-char (count &optional crosslines noerror)
  "Bidi-aware evil motion."
  :type exclusive
  (interactive "<c>" (list evil-cross-lines
                           (evil-kbd-macro-suppress-motion-error)))
  (pcase (current-bidi-paragraph-direction)
    (`left-to-right
     (evil-forward-char count crosslines noerror))
    (`right-to-left
     (evil-backward-char count crosslines noerror))))

(evil-define-motion evil-bidi-first-non-blank ()
  "Bidi-aware evil motion."
  :type exclusive
  (pcase (current-bidi-paragraph-direction)
    (`left-to-right
     (evil-first-non-blank))
    (`right-to-left
     (evil-end-of-line))))

(evil-define-motion evil-bidi-end-of-line ()
  "Bidi-aware evil motion."
  :type exclusive
  (pcase (current-bidi-paragraph-direction)
    (`left-to-right
     (evil-end-of-line))
    (`right-to-left
     (evil-first-non-blank))))

(evil-global-set-key 'motion [remap evil-forward-char] #'evil-bidi-forward-char)
(evil-global-set-key 'motion [remap evil-backward-char] #'evil-bidi-backward-char)
(evil-global-set-key 'motion [remap evil-first-non-blank] #'evil-bidi-first-non-blank)
(evil-global-set-key 'motion [remap evil-end-of-line] #'evil-bidi-end-of-line)

(provide 'evil-bidi)

;;; evil-bidi.el ends here
