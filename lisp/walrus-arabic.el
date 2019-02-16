;;; walrus-arabic.el --- Quail package for inputting Arabic	-*- coding: utf-8;-*-
;;; Commentary:
;;; Code:

(require 'quail)

(quail-define-package
 "walrus-arabic" "Arabic" "ع" nil "Arabic input method.

A customised version of the Arabic input method.
" nil t t t t nil nil nil nil nil t)

(quail-define-rules
 (";" ?؛)
 ("." ?.)
 ("," ?،)
 ("_" ?ـ)
 ("?" ?؟)
 ("<" ?«)
 (">" ?»)

 ("1" ?١)
 ("2" ?٢)
 ("3" ?٣)
 ("4" ?٤)
 ("5" ?٥)
 ("6" ?٦)
 ("7" ?٧)
 ("8" ?٨)
 ("9" ?٩)
 ("0" ?٠)

 ("@" ?َ)
 ("^" ?ِ)
 ("$" ?ُ)
 ("%" ?ْ)
 ("Z" ?ّ)
 ("*" ?ً)
 ("+" ?ٍ)
 ("-" ?ٌ)

 ("a" ?ا)
 ("A" ?آ)
 ("o" ?أ)
 ("e" ?ة)
 ("E" ?ء)
 ("i" ?إ)
 ("I" ?ئ)
 ("u" ?أ)
 ("U" ?ؤ)


 ("p" ?ص)
 ("P" ?ض)

 ("y" ?ي)
 ("Y" ?ى)

 ("f" ?ف)
 ("g" ?ع)
 ("G" ?غ)
 ("c" ?ط)
 ("C" ?ظ)
 ("r" ?ر)
 ("l" ?ل)
 ("L" ["لا"])

 ("d" ?د)
 ("D" ?ذ)
 ("h" ?ه)
 ("H" ?ح)
 ("t" ?ت)
 ("T" ?ث)
 ("n" ?ن)
 ("N" ["نعم"])
 ("s" ?س)
 ("S" ?ش)

 ("q" ?ق)
 ("j" ?ج)
 ("J" ?چ)
 ("k" ?ك)
 ("K" ?گ)
 ("x" ?خ)

 ("b" ?ب)
 ("B" ?پ)
 ("m" ?م)
 ("w" ?و)
 ("v" ?ﷲ)
 ("z" ?ز)
 ("Z" ?ژ))

(provide 'walrus-arabic)

;;; walrus-arabic.el ends here
