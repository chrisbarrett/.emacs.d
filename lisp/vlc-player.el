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



(defconst vlc-player-buffer-name "*vlc*")

(defconst vlc-player-mode-prompt-regexp (rx bol "> "))

(defconst vlc-player-header-line-format
  '((:eval (substitute-command-keys "Type \\[vlc-player-cmd-play-or-pause] to play/pause, \\[vlc-player-cmd-stop] to stop, and \\[vlc-player-bury-window] to exit"))))

;;;###autoload
(defconst vlc-player-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-SPC") #'vlc-player-cmd-play-or-pause)
    (define-key keymap (kbd "C-c SPC") #'vlc-player-cmd-play-or-pause)
    (define-key keymap (kbd "C-c C-.") #'vlc-player-cmd-stop)
    (define-key keymap (kbd "C-c .") #'vlc-player-cmd-stop)
    (define-key keymap (kbd "C-c o") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c C-o") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c C-f") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c f") #'vlc-player-play-file)
    (define-key keymap (kbd "C-c C-q") #'vlc-player-bury-window)
    (define-key keymap (kbd "C-c q") #'vlc-player-bury-window)
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

(defun vlc-player-cmd-stop ()
  "Stop the running player."
  (interactive)
  (vlc-player-send-command "stop")
  (message "Player stopped"))

(defun vlc-player-cmd-play-or-pause ()
  "Play or pause the running player."
  (interactive)
  (vlc-player-send-command "pause")
  (message "Playing/pausing"))

;;;###autoload
(defun vlc-player-play-file (file)
  "Play FILE in an inferior VLC player."
  (interactive "fFile: ")
  (vlc-player-buffer t)
  (vlc-player-send-command "stop")
  (vlc-player-send-command "clear")
  (vlc-player-send-command "add" (expand-file-name file))
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

(defun vlc-player-send-command (command &rest args)
  (if-let* ((buf (vlc-player-buffer))
            (proc (get-buffer-process buf)))
      (with-current-buffer buf
        (let ((str (string-join (cons command args) " ")))
          (goto-char (point-max))
          (comint-delete-input)
          (insert str)
          (comint-send-input)
          str))
    (error "VLC not running--check the *vlc* buffer")))



;;;###autoload
(defun vlc-player ()
  "Start or show the inferior VLC player."
  (interactive)
  (when-let* ((win (display-buffer (vlc-player-buffer t))))
    (select-window win)))

(provide 'vlc-player)

;;; vlc-player.el ends here
