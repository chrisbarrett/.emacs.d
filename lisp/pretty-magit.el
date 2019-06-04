;;; pretty-magit.el --- Fontifications for magit  -*- lexical-binding: t; -*-

;; Author: Eric Kaschalk

;;; Commentary:

;; Source:
;;   https://github.com/ekaschalk/.spacemacs.d/blob/master/layers/display/local/pretty-magit/pretty-magit.el
;; See also:
;;   http://www.modernemacs.com/post/pretty-magit/

;;; Code:

(require 'ivy)
(require 'magit)

(provide 'pretty-magit)

;;; Pretty-magit

(defvar pretty-magit-alist nil
  "An alist of regexes, an icon, and face properties to apply to icon.")

;;;###autoload
(defun pretty-magit-add-leader (regex replacement props &optional modes)
  "Replace REGEX with REPLACEMENT with PROPS."
  (add-to-list 'pretty-magit-alist (list regex replacement props modes)))

(defun pretty-magit--apply-face (start end icon props)
  (when icon
    (compose-region start end icon))
  (when props
    (add-face-text-property start end props)))

;;;###autoload
(defun pretty-magit-add-magit-faces (&rest _)
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let* (((rgx icon props modes) it)
              (should-apply-p (or (null modes) (apply 'derived-mode-p modes))))
        (when should-apply-p
          (save-excursion
            (goto-char (point-min))
            (while (search-forward-regexp rgx nil t)
              (let ((start (or (match-beginning 1)
                               (match-beginning 0)))
                    (end (or (match-end 1)
                             (match-end 0))))
                (pretty-magit--apply-face start end icon props)))))))))

(advice-add 'magit-status :after 'pretty-magit-add-magit-faces)
(advice-add 'magit-refresh-buffer :after 'pretty-magit-add-magit-faces)

(provide 'pretty-magit)

;;; pretty-magit.el ends here
