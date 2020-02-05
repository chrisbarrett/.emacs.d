;;; files-hacks.el --- Hacks for files.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'el-patch)
(require 'f)

(el-patch-feature 'files)

(defconst files-hacks--archive-file-extensions '("gz" "tgz" "zip"))

;; Don't prompt to open literally when trying to open large archive files.

(with-eval-after-load 'files
  (el-patch-defun abort-if-file-too-large (size op-type filename &optional offer-raw)
    "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
OP-TYPE specifies the file operation being performed (for message
to user).  If OFFER-RAW is true, give user the additional option
to open the file literally.  If the user chooses this option,
`abort-if-file-too-large' returns the symbol `raw'.  Otherwise,
it returns nil or exits non-locally."
    (let ((choice (and large-file-warning-threshold size
                       (> size large-file-warning-threshold)
                       ;; No point in warning if we can't read it.
                       (file-readable-p filename)
                       (el-patch-add (not (seq-contains-p files-hacks--archive-file-extensions (f-ext filename))))
                       (files--ask-user-about-large-file
                        size op-type filename offer-raw))))
      (when (eq choice 'abort)
        (user-error "Aborted"))
      choice)))

(provide 'files-hacks)

;;; files-hacks.el ends here
