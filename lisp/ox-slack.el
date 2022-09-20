;;; ox-slack.el --- Export backend for Slack-style markdown.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ox)
(require 'ox-gfm)
(require 'subr-x)

(autoload 'gfm-mode "markdown-mode")

(defgroup ox-slack nil
  "Export backend for Slack markup."
  :group 'languages
  :prefix "ox-slack-")


(defcustom ox-slack-postprocess-function #'identity
  "Function for transforming the output of the exported org tree.

Useful for replacing references to people with username, etc."
  :type 'function)

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

(defun ox-slack--passthrough (_ contents _info)
  (format "%s" contents))

(defun ox-slack--italic (_italic contents _info)
  (format "_%s_" contents))

(defun ox-slack--bold (_bold contents _info)
  (format "*%s*" contents))

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
              (`on "`[X]` ")
              (`trans "`[-]` ")
              (`off "`[ ]` "))
            (let ((tag (org-element-property :tag item)))
              (and tag (format "*%s:* "(org-export-data tag info))))
            (and contents
                 (org-trim (replace-regexp-in-string "^" "    " contents))))))

(defun ox-slack--link (&rest args)
  (pcase-let ((`(,link ,_ ,_) args))
    (apply (if (string-match-p (rx bol "id:") link)
               #'ox-slack--passthrough
             #'org-md-link)
           args)))

(defun ox-slack--fixed-width-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "```\n%s\n```"
          (org-remove-indentation
           (org-export-format-code-default example-block info))))

(defun ox-slack--timestamp (timestamp _contents _info)
  (org-timestamp-translate timestamp))

(org-export-define-derived-backend 'slack 'gfm
  :translate-alist '((bold . ox-slack--bold)
                     (example-block . ox-slack--fixed-width-block)
                     (fixed-width . ox-slack--fixed-width-block)
                     (headline . ox-slack--markup-headline)
                     (italic . ox-slack--italic)
                     (item . ox-slack--item)
                     (link . ox-slack--link)
                     (src-block . ox-slack--fixed-width-block)
                     (strike-through . ox-slack--strike-through)
                     (timestamp . ox-slack--timestamp)
                     (subscript . ox-slack--passthrough)
                     (superscript . ox-slack--passthrough)
                     (underline . ox-slack--italic))
  :menu-entry
  '(?s "Export to Slack Markup"
       ((?c "To clipboard" ox-slack-export-to-clipboard)
        (?s "To temporary buffer" ox-slack-export-to-buffer))))

(defmacro ox-slack--with-default-export-options (&rest body)
  (declare (indent 0))
  `(let ((org-export-with-author nil)
         (org-export-with-toc nil)
         (org-export-with-creator nil)
         (org-html-special-string-regexps nil)
         (org-export-with-email nil))
     ,@body))

(defun ox-slack-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (ox-slack--with-default-export-options
    (org-export-to-buffer 'slack "*Org Slack Export*"
      async subtreep visible-only body-only ext-plist
      (lambda ()
        (let ((str (funcall ox-slack-postprocess-function (buffer-string))))
          (erase-buffer)
          (insert str)
          (gfm-mode))))))

(defun ox-slack-export-to-clipboard (&optional async subtreep visible-only body-only ext-plist formatter)
  (interactive)
  (let ((org-export-show-temporary-export-buffer nil))
    (ox-slack--with-default-export-options
      (org-export-to-buffer 'slack "*Org Slack Export*"
        async subtreep visible-only body-only ext-plist
        (lambda ()
          (let* ((str (funcall ox-slack-postprocess-function (string-trim (buffer-string))))
                 (postprocessed (funcall (or formatter #'identity) str)))
            (kill-new postprocessed))
          (message "Buffer contents copied to clipboard"))))))

(provide 'ox-slack)

;;; ox-slack.el ends here
