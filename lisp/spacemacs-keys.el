;;; spacemacs-keys.el --- Spacemacs Core File

;; Copyright (c) 2012-2016 Sylvain Benner & Contributors

;; Author: Sylvain Benner <sylvain.benner@gmail.com>

;;; Commentary:

;; Key binding utilities ported from Spacemacs.

;;; Code:

(autoload 'evil-normalize-keymaps "evil")
(autoload 'which-key-declare-prefixes "which-key")
(autoload 'which-key-declare-prefixes-for-mode "which-key")
(autoload 'evil-global-set-key "evil")

(require 'bind-map)

(defgroup spacemacs-keys nil
  "Ports the core Spacemacs key binding utilities."
  :group 'editing
  :prefix "spacemacs-keys-")

(defcustom spacemacs-keys-leader-key "SPC"
  "The primary leader key to use."
  :group 'spacemacs-keys
  :type 'string)

(defvar spacemacs-keys-prefix-titles nil
  "Alist for mapping command prefixes to long names.")

(defvar spacemacs-keys-default-map (make-sparse-keymap)
  "Base keymap for all Spacemacs leader key commands.")

(defun spacemacs-keys-declare-prefix (prefix name &optional long-name)
  "Declare a prefix to document a group of key bindings for which-key.

PREFIX is a string describing a key sequence.

NAME is a string used as the prefix command.

LONG-NAME if given is stored in `spacemacs-keys-prefix-titles'."
  (let* ((command name)
         (full-prefix (concat spacemacs-keys-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-declare-prefixes
      full-prefix (cons name long-name))))

;;;###autoload
(defun spacemacs-keys-set-leader-keys (key def &rest bindings)
  "Define key bindings under the leader prefix.
Bindings defined with this routine will be available under
`spacemacs-keys-leader-key'.

KEY is the key to bind under the leader.  It is a string that will
be used as an argument to `kbd'.

DEF is the command to be called.

BINDINGS are any additional keys and defs to bind."
  (while key
    (define-key spacemacs-keys-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'spacemacs-keys-set-leader-keys 'lisp-indent-function 'defun)

(bind-map spacemacs-keys-default-map
  :prefix-cmd spacemacs-keys-cmds
  :evil-keys (spacemacs-keys-leader-key)
  :override-minor-modes t
  :override-mode-name spacemacs-keys-leader-override-mode)

(provide 'spacemacs-keys)

;;; spacemacs-keys.el ends here
