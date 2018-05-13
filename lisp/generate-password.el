;;; generate-password.el --- A hydra for generating passwords.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'hydra)
(require 's)
(require 'subr-x)


;; Internal flag state

(defvar generate-password--length 30)

(defvar generate-password--special-chars-p t)

(defun generate-password--state-to-command ()
  (let* ((alnum "A-Za-z0-9")
         (chars (concat alnum (when generate-password--special-chars-p "!\"#$%&'\''()*+,-./:;<=>?@[\]^_`{|}~"))))
    (format "</dev/urandom tr -dc %s | head -c %s"
            (shell-quote-argument chars)
            generate-password--length)))


;; Hydra definition

(defun generate-password--run (command)
  (string-trim (shell-command-to-string command)))

(defun generate-password--read-length ()
  (let ((updated (read-number "Length: " generate-password--length)))
    (unless (< 0 updated)
      (user-error "Number must be greater than 0"))
    (setq generate-password--length updated)))

(defun generate-password--format-preview ()
  (let ((password (s-replace "%" "%%" (generate-password--run (generate-password--state-to-command)))))
    (propertize password 'face 'font-lock-comment-face)))

;;;###autoload
(defhydra generate-password (:color amaranth :hint nil)
  "
Generate a password using /dev/urandom.

     %s(propertize \"ᕕ( ᐛ )ᕗ\" 'face '(:foreground \"gray50\"))

Example with current settings:

     %s(generate-password--format-preview)


^Flags^           ^^^^ Value     ^^^^^^^^^^^^^^^^Actions
^^----------------^^^^ -----     ^^^^^^^^^^^^^^^^--------------------
^^_l_: length          % 3`generate-password--length     ^^^^^^^^^_i_: insert
^^_!_: special chars   % 3`generate-password--special-chars-p     _y_: copy to kill-ring
^^                           ^^^^^^^^^^^^^^^^^^^^_q_: quit
"
  ("l" (generate-password--read-length))
  ("!" (setq generate-password--special-chars-p (not generate-password--special-chars-p)))
  ("y" (progn (kill-new (generate-password--run (generate-password--state-to-command))) (message "Password copied to kill-ring.")) :exit it)
  ("i" (insert (generate-password--run (generate-password--state-to-command))) :exit t)
  ("q" nil :exit t))

(provide 'generate-password)

;;; generate-password.el ends here
