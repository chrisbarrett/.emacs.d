;;; treemacs-hacks.el --- Hacks for treemacs.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'el-patch)
(require 'paths)

(el-patch-feature treemacs)
(el-patch-feature treemacs-persistence)



;; Error persist file is set as defconst and referenced during load. Fix value.

(with-eval-after-load 'treemacs-persistence
  (el-patch-defconst treemacs--last-error-persist-file
    (el-patch-swap
      (f-join user-emacs-directory ".cache" "treemacs-persist-at-last-error")
      (f-join paths-cache-directory "treemacs" "treemacs-errors"))
    "File that stores the treemacs state as it was during the last load error."))

(provide 'treemacs-hacks)

;;; treemacs-hacks.el ends here
