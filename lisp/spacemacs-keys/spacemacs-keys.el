;;; spacemacs-keys.el --- Spacemacs Core File

;; Copyright (c) 2012-2016 Sylvain Benner & Contributors

;; Author: Sylvain Benner <sylvain.benner@gmail.com>

;;; Commentary:

;; Key binding utilities ported from Spacemacs.

;;; Code:

(autoload 'evil-normalize-keymaps "evil-core")
(autoload 'which-key-declare-prefixes "which-key")
(autoload 'which-key-declare-prefixes-for-mode "which-key")
(autoload 'evil-global-set-key "evil-core")

(require 'bind-map)

(defgroup spacemacs-keys nil
  "Ports the core Spacemacs key binding utilities."
  :group 'editing
  :prefix "spacemacs-keys-")

(defcustom spacemacs-keys-leader-key "SPC"
  "The primary leader key to use."
  :group 'spacemacs-keys
  :type 'string)

(defcustom spacemacs-keys-major-mode-leader-key ","
  "Major mode leader key is a shortcut equivalent to `<leader> m`."
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

(defun spacemacs-keys-declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix for a particular major mode.

MODE is the mode in which this prefix command should be added.

PREFIX is a string describing a key sequence.

NAME is a symbol name used as the prefix command.

LONG-NAME is an optional longer name for the prefix."
  (let ((command (intern (concat (symbol-name mode) name)))
        (full-prefix (concat spacemacs-keys-leader-key " " prefix))
        (is-major-mode-prefix (string-prefix-p "m" prefix))
        (major-mode-prefix (concat spacemacs-keys-major-mode-leader-key " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes-for-mode mode
        full-prefix prefix-name)
      (when (and is-major-mode-prefix spacemacs-keys-major-mode-leader-key)
        (which-key-declare-prefixes-for-mode mode major-mode-prefix prefix-name)))))

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

(defalias 'evil-leader/set-key 'spacemacs-keys-set-leader-keys)

(defun spacemacs-keys--acceptable-leader-p (key)
  (and (stringp key) (not (string= key ""))))

(defun spacemacs-keys--init-leader-mode-map (mode map &optional minor)
  "Prepare a keymap for use with leader commands.

MODE is the major or minor mode to restrict the bindings to.

MAP is the keymap to prepare.

If MODE is a minor mode, MINOR should be non-nil."
  ;; Checks for MAP-prefix.  If it doesn't exist yet, use `bind-map'
  ;; to create it and bind it to `spacemacs-keys-major-mode-leader-key'.
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (spacemacs-keys--acceptable-leader-p
                         spacemacs-keys-major-mode-leader-key)
                    spacemacs-keys-major-mode-leader-key))
         (leader2 (when (spacemacs-keys--acceptable-leader-p
                         spacemacs-keys-leader-key)
                    (concat spacemacs-keys-leader-key (unless minor " m"))))
         (leaders (delq nil (list leader1 leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :evil-keys ,leaders
              :evil-states (normal motion visual evilified)))
          (boundp prefix)))))

;;;###autoload
(defun spacemacs-keys-set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Define key bindings available for a certain major mode.

MODE is the major mode.

KEY is the key to bind under `spacemacs-keys-major-mode-leader-key'.

DEF is the command to call.

BINDINGS are additional key and def pairs to bind."
  (let ((map (intern (format "spacemacs-keys-%s-map" mode))))
    (when (spacemacs-keys--init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'spacemacs-keys-set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defalias
  'evil-leader/set-key-for-mode
  'spacemacs-keys-set-leader-keys-for-major-mode)

;;;###autoload
(defun spacemacs-keys-set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add leader keys for a minor mode.

MODE is the minor mode.

KEY is the key to bind under the major mode leader.

DEF is the command to be called.

BINDINGS is a list of more KEY and DEF pairs."
  (let ((map (intern (format "spacemacs-keys-%s-map" mode))))
    (when (spacemacs-keys--init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'spacemacs-keys-set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)


(bind-map spacemacs-keys-default-map
  :prefix-cmd spacemacs-keys-cmds
  :evil-keys (spacemacs-keys-leader-key)
  :override-minor-modes t
  :override-mode-name spacemacs-keys-leader-override-mode)


(provide 'spacemacs-keys)

;;; spacemacs-keys.el ends here
