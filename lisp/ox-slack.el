;;; ox-slack.el --- Export backend for Slack-style markdown.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ox)
(require 'ox-md)
(require 'subr-x)

(autoload 'gfm-mode "markdown-mode")

(defun ox-slack--markup-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (todo (org-export-data (org-element-property :todo-keyword headline) info))
         (todo-text (unless (or (not (plist-get info :with-todo-keywords))
                                (string= todo ""))
                      todo)))
    (concat
     (when todo-text (concat todo-text " "))
     "*" text "*"
     "\n\n" (when (org-string-nw-p contents) contents))))

(defun ox-slack--italic (_italic contents _info)
  (format "_%s_" contents))

(defun ox-slack--bold (_bold contents _info)
  (format "*%s*" contents))

(defun ox-slack--link (link _contents _info)
  (org-element-property :raw-link link))

(defun ox-slack--strike-through (_italic contents _info)
  (format "~%s~" contents))

(defun ox-slack--item (item contents info)
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "-"
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           "."))))
    (concat bullet
            " "
            (pcase (org-element-property :checkbox item)
              (`on "[X] ")
              (`trans "[-] ")
              (`off "[ ] "))
            (let ((tag (org-element-property :tag item)))
              (and tag (format "*%s:* "(org-export-data tag info))))
            (and contents
                 (org-trim (replace-regexp-in-string "^" "    " contents))))))

(defun ox-slack--fixed-width-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "```\n%s\n```"
          (org-remove-indentation
           (org-export-format-code-default example-block info))))

(org-export-define-derived-backend 'slack 'gfm
  :translate-alist '((headline . ox-slack--markup-headline)
                     (toc . ox-slack--toc)
                     (link . ox-slack--link)
                     (item . ox-slack--item)
                     (italic . ox-slack--italic)
                     (underline . ox-slack--italic)
                     (bold . ox-slack--bold)
                     (strike-through . ox-slack--strike-through)
                     (example-block . ox-slack--fixed-width-block)
                     (fixed-width . ox-slack--fixed-width-block)
                     (src-block . ox-slack--fixed-width-block))
  :menu-entry
  '(?s "Export to Slack Markup"
       ((?c "To clipboard"
            (lambda (a s v b) (ox-slack-export-to-clipboard a s v)))
        (?s "To temporary buffer"
            (lambda (a s v b) (ox-slack-export-to-buffer a s v))))))

(defun ox-slack-export-to-buffer (&optional async subtreep visible-only)
  "Export the buffer to Slack markup.

ASYNC, SUBTREEP and VISIBLE-ONLY are as specified in the export dispatcher."
  (interactive)
  (org-export-to-buffer 'slack "*Org Slack Export*"
    async subtreep visible-only nil nil (lambda () (gfm-mode))))

(defun ox-slack-export-to-clipboard (&optional async subtreep visible-only formatter)
  "Export the buffer to Slack markup.

ASYNC, SUBTREEP and VISIBLE-ONLY are as specified in the export dispatcher.

FORMATTER allows you to modify the output string before copying."
  (interactive)
  (let ((org-export-show-temporary-export-buffer nil))
    (org-export-to-buffer 'slack "*Org Slack Export*"
      async subtreep visible-only nil nil (lambda ()
                                            (kill-new (funcall (or formatter #'string-trim) (buffer-string)))
                                            (message "Buffer contents copied to clipboard")))))

(provide 'ox-slack)

;;; ox-slack.el ends here
