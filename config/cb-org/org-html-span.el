;;; org-html-span.el --- Enrich org links with spans for html export.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Sourced from https://korewanetadesu.com/org-mode-spans.html

;;; Code:

(autoload 'org-link-set-parameters "org")
(autoload 's-replace-all "s")

;; http://www.w3.org/TR/2009/WD-html5-20090212/serializing-html-fragments.html
;;
;; "Escaping a string... consists of replacing any occurrences of
;; the "&" character by the string "&amp;", any occurrences of the
;; U+00A0 NO-BREAK SPACE character by the string "&nbsp;", and, if
;; the algorithm was invoked in the attribute mode, any occurrences
;; of the """ character by the string "&quot;"..."

(defvar org-html-span--entity-conversion-alist
  '(("&" . "&amp;")
    ("\u00a0" . "&nbsp;")
    ("\"" . "&quot;")))

(defun org-html-span--escape-attribute (value)
  (s-replace-all org-html-span--entity-conversion-alist value))

(defun org-html-span--export-span (class desc format)
  (pcase format
    (`html
     (format "<span class=\"%s\">%s</span>"
             (org-html-span--escape-attribute class)
             (or desc "")))
    (_
     (or desc ""))))

(with-eval-after-load 'org
  (org-link-set-parameters "span" :export #'org-html-span--export-span))

(provide 'org-html-span)

;;; org-html-span.el ends here
