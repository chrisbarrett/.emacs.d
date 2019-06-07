;;; org-hacks.el --- Hacks to support org-mode configuration. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'org
  (unless (fboundp 'org-make-tag-string)
    (defun org-make-tag-string (tags)
      "Return string associated to TAGS.
TAGS is a list of strings."
      (if (null tags) ""
        (format ":%s:" (mapconcat #'identity tags ":"))))))

(provide 'org-hacks)

;;; org-hacks.el ends here
