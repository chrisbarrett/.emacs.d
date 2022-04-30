;;; vlc-player.el --- Rudimentary VLC playback hosted by Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Chris Barrett

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

;; Rudimentary VLC player implementation.

;;; Code:

(require 'dash)
(require 'comint)
(require 'subr-x)

(defgroup vlc-player nil
  "VLC playback hosted by Emacs."
  :group 'media
  :prefix "vlc-player-")

(defcustom vlc-player-executable
  (or (executable-find "cvlc")
      (let ((mac-app-bin "/Applications/VLC.app/Contents/MacOS/VLC"))
        (when (and (eq system-type 'darwin) (file-exists-p mac-app-bin))
          mac-app-bin)))
  "The path to the VLC player executable."
  :group 'vlc-player
  :type 'file)

(defcustom vlc-player-seek-forward-seconds 5
  "The number of seconds to advance when seekping forward."
  :group 'vlc-player
  :type 'numberp)

(defcustom vlc-player-seek-backward-seconds 5
  "The number of seconds to go back when seekping back."
  :group 'vlc-player
  :type 'numberp)



(defconst vlc-player-buffer-name "*vlc*")

(defconst vlc-player-mode-prompt-regexp (rx bol "> "))

(defconst vlc-player-header-line-format
  '((:eval (substitute-command-keys "\\[vlc-player-play-file] to play a file, \\[vlc-player-cmd-play-or-pause] to play/pause, \\[vlc-player-stop-and-bury] to exit, \\[vlc-player-bury-window] to hide the window, \\[vlc-player-cmd-seek-forward] to seek forward and \\[vlc-player-cmd-seek-backward] to seek backward."))))

;;;###autoload
(defconst vlc-player-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-SPC") #'vlc-player-cmd-play-or-pause)
    (define-key keymap (kbd "C-c SPC") #'vlc-player-cmd-play-or-pause)
    (define-key keymap (kbd "C-c C-k") #'vlc-player-stop-and-bury)
    (define-key keymap (kbd "C-c o") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c C-o") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c C-f") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c f") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c C-q") #'vlc-player-bury-window)
    (define-key keymap (kbd "C-c q") #'vlc-player-bury-window)
    (define-key keymap (kbd "C-c C-h") #'vlc-player-cmd-seek-backward)
    (define-key keymap (kbd "C-h") #'vlc-player-cmd-seek-backward)
    (define-key keymap (kbd "C-c h") #'vlc-player-cmd-seek-backward)
    (define-key keymap (kbd "C-l") #'vlc-player-cmd-seek-forward)
    (define-key keymap (kbd "C-c C-l") #'vlc-player-cmd-seek-forward)
    (define-key keymap (kbd "C-c l") #'vlc-player-cmd-seek-forward)
    keymap))

(defun vlc-player-bury-window (&optional kill)
  "Bury the inferior VLC player.

With prefix arg KILL, also quit VLC."
  (interactive "P")
  (let ((action))
    (when-let* ((buf (get-buffer vlc-player-buffer-name)))
      (ignore-errors
        (setq action 'bury)
        (mapc #'delete-window (get-buffer-window-list buf)))
      (when kill
        (setq action 'kill)
        (let ((kill-buffer-query-functions))
          (kill-buffer buf))))
    (pcase action
      ('bury
       (message "VLC buffer buried"))
      ('kill
       (message "Exited VLC")))))

;;;###autoload
(define-derived-mode vlc-player-mode comint-mode " VLC Player"
  "Minor mode for buffers with a VLC player session."
  (setq-local comint-prompt-read-only t)
  (setq-local comint-prompt-regexp vlc-player-mode-prompt-regexp))

(defun vlc-player-stop-and-bury (&optional kill)
  "Stop the running player and hide the player window.

With prefix arg KILL, also exit VLC."
  (interactive "P")
  (vlc-player-execute `(stop))
  (vlc-player-bury-window kill))

(defun vlc-player-cmd-play-or-pause ()
  "Play or pause the running player."
  (interactive)
  (vlc-player-execute `(pause))
  (message "Playing/pausing"))

(defun vlc-player-cmd-seek-forward ()
  "Step forward when playing.

The amount to seek each keypress can be customized with
`vlc-player-seek-forward-seconds'."
  (interactive)
  (vlc-player-execute `(seek ,(format "+%s" vlc-player-seek-forward-seconds))))

(defun vlc-player-cmd-seek-backward ()
  "Step backward when playing.

The amount to seek each keypress can be customized with
`vlc-player-seek-backward-seconds'."
  (interactive)
  (vlc-player-execute `(seek ,(format "-%s" vlc-player-seek-backward-seconds))))

(defun vlc-player-cmd-seek (seconds)
  "Seek to SECONDS in the currently playing stream."
  (interactive "nSeconds: ")
  (vlc-player-execute `(seek ,(number-to-string seconds))))

;;;###autoload
(defun vlc-player-play-file (file &optional start)
  "Play FILE in an inferior VLC player.

START, if given, is the time in the stream (in seconds) to start at."
  (interactive "fFile: ")
  (vlc-player-buffer t)
  (vlc-player-execute `((clear)
                        (add ,(expand-file-name file))
                        ,(when start `(seek ,start))))
  (message "Playing %s" (abbreviate-file-name file)))

(defun vlc-player--setup-buffer ()
  (unless (comint-check-proc (current-buffer))
    (unless (derived-mode-p 'vlc-player-mode)
      (vlc-player-mode))
    (comint-exec (current-buffer) "vlc"  vlc-player-executable nil '("-I" "rc")))
  (setq-local header-line-format vlc-player-header-line-format)
  (force-mode-line-update))

(defun vlc-player-buffer (&optional startup-enabled)
  (with-current-buffer (get-buffer-create vlc-player-buffer-name)
    (when startup-enabled
      (vlc-player--setup-buffer))
    (current-buffer)))

(defun vlc-player--send-command-string (command-string)
  (if-let* ((buf (vlc-player-buffer))
            (proc (get-buffer-process buf)))
      (with-current-buffer buf
        (goto-char (point-max))
        (comint-delete-input)
        (insert command-string)
        (comint-send-input))
    (error "VLC not running--check the *vlc* buffer")))

(defun vlc-player--parse-command-or-commands (command-or-commands)
  (--> command-or-commands
       (if (listp (car it)) it (list it))
       (-non-nil it)
       (seq-map (lambda (sexp)
                  (string-join (--map (format "%s" it) sexp) " "))
                it)))

(defun vlc-player-execute (command-or-commands)
  "Send the inferior VLC process commands to execute together.

COMMAND-OR-COMMANDS is a S-Expression. If the first entry is a
symbol, it is interpreted to be a single command. If the first
value is a list, it is interpreted to mean a list of commands to
be and-ed and executed together."
  (dolist (command-string (vlc-player--parse-command-or-commands command-or-commands))
    (vlc-player--send-command-string command-string)
    ;; KLUDGE: Give VLC a moment to recieve each command.
    (sit-for 0.01)))



;;;###autoload
(defun vlc-player ()
  "Start or show the inferior VLC player."
  (interactive)
  (when-let* ((win (display-buffer (vlc-player-buffer t))))
    (select-window win)))

(provide 'vlc-player)

;;; vlc-player.el ends here
