;;; stack-hoogle.el --- Adapt haskell-hoogle to use stack.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'haskell-hoogle)

(defun stack-hoogle--read-query ()
  (let ((def (haskell-ident-at-point)))
    (read-string (if def (format "Hoogle query (default %s): " def) "Hoogle query: ")
                 nil nil def)))

(defun stack-hoogle--string-is-haskell-source? (s)
  (or (s-contains? "::" s)
      (s-matches? (rx bol (or "class" "module" "data") eow) s)))

(defun stack-hoogle--indent-section (start end)
  (indent-region start end 2)
  (goto-char end)
  (fill-region start end))

(defun stack-hoogle--insert-header (s)
  (newline)
  (insert (propertize s 'font-lock-face 'font-lock-comment-face))
  (newline))

(defun stack-hoogle--prettify-hoogle-info-results ()
  (goto-char (point-min))
  (unless (s-matches? "No results found" (buffer-substring (line-beginning-position) (line-end-position)))
    (save-excursion
      (stack-hoogle--insert-header "Definition")
      (newline)
      (stack-hoogle--indent-section (line-beginning-position) (line-end-position))
      (goto-char (line-end-position))
      (newline)

      (stack-hoogle--insert-header "Module")
      (forward-line)
      (stack-hoogle--indent-section (line-beginning-position) (line-end-position))
      (goto-char (line-end-position))

      (unless (s-blank? (s-trim (buffer-substring (point) (point-max))))
        (newline)
        (stack-hoogle--insert-header "Description")))))

;;;###autoload
(defun stack-hoogle (query &optional info)
  "Do a Hoogle search for QUERY.

If prefix argument INFO is given, then hoogle is asked to show
extra info for the items matching QUERY.."
  (interactive (list (stack-hoogle--read-query) current-prefix-arg))
  (let ((command (format "stack hoogle -- --colour %s %s"
                         (if info " -i " "")
                         (shell-quote-argument query))))
    (with-help-window "*stack hoogle*"
      (with-current-buffer standard-output
        (prettify-symbols-mode +1)
        (insert (shell-command-to-string command))
        (when info
          (stack-hoogle--prettify-hoogle-info-results))))))

;;;###autoload
(defun stack-hoogle-info-at-pt ()
  "Show info for the identifier at point using Hoogle."
  (interactive)
  (-if-let (query (haskell-ident-at-point))
      (stack-hoogle query t)
    (user-error "No identifier at point")))

(provide 'stack-hoogle)

;;; stack-hoogle.el ends here
