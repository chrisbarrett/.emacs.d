;;; generate-password.el --- A hydra for generating passwords.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Chris Barrett
;; Package-Requires: ((emacs "24.4") (hydra "0.13.6"))
;; Version: 0.1

;; Author: Chris Barrett <chris@walrus.cool>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'hydra)
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

;;;###autoload
(defhydra generate-password (:color amaranth :hint nil)
  "
Generate a password using /dev/urandom.

     %s(propertize \"ᕕ( ᐛ )ᕗ\" 'face '(:foreground \"gray50\"))

Preview:

     %s(propertize (generate-password--run (generate-password--state-to-command)) 'face 'font-lock-comment-face)


^Flags^           ^^^^ Value     ^^^^^^^^^^^^^^^^Actions
^^----------------^^^^ -----     ^^^^^^^^^^^^^^^^--------------------
^^_l_: length          % 3`generate-password--length     ^^^^^^^^^_i_: insert
^^_!_: special chars   % 3`generate-password--special-chars-p     _y_: copy to kill-ring
^^                           ^^^^^^^^^^^^^^^^^^^^_q_: quit
"
  ("l" (generate-password--read-length) :color red)

  ("!" (setq generate-password--special-chars-p (not generate-password--special-chars-p)) :color red)

  ("y"
   (progn
     (kill-new (generate-password--run (generate-password--state-to-command)))
     (message "Password copied to kill-ring."))
   :color blue)

  ("i"
   (insert (generate-password--run (generate-password--state-to-command)))
   :color blue)

  ("q" nil))


(defalias 'generate-password #'generate-password/body)

(provide 'generate-password)

;;; generate-password.el ends here
