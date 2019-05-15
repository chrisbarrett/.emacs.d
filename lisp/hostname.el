;;; hostname.el --- Utilty to compute the current hostname.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)

(defun hostname ()
  (cadr (s-match (rx (group (+? nonl)) (? "-" (+ digit)) (? ".local") eos)
                 (downcase (system-name)))))

(provide 'hostname)

;;; hostname.el ends here
