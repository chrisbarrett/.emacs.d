;;; config-nixos.el --- NixOS-specific configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'awesomewm)
(require 'dash-functional)
(require 'general)
(require 'subr-x)

(general-define-key "s-c" #'clipboard-kill-ring-save)
(general-define-key "s-v" #'clipboard-yank)
(general-define-key "<s-return>" #'toggle-frame-fullscreen)



(defconst config-nixos--compile-bufname "*nixos-build*")

(defun config-nixos-build ()
  "Build the NixOS environment."
  (interactive)
  (let* ((default-directory "~/Sync/nix")
         (compilation-buffer-name-function (-const config-nixos--compile-bufname))

         (compilation-process-setup-function
          (lambda ()
            (with-current-buffer (get-buffer-create config-nixos--compile-bufname)
              (when-let* ((proc (get-buffer-process (current-buffer))))
                (awesomewm-notify "NixOS build" "Starting build...")))))

         (on-complete
          (lambda (buf msg)
            (with-current-buffer buf
              (pcase (string-trim msg)
                ((and "finished" (guard (string-match-p (rx bol "make: Nothing to be done for 'install'") (buffer-string))))
                 (awesomewm-notify "NixOS build" "No changes made to system."))
                ("finished"
                 (awesomewm-notify "NixOS build" "Finished successfully."))
                (_
                 (awesomewm-notify-error "NixOS build" "Aborted with errors."))))))

         (buf (get-buffer-create config-nixos--compile-bufname)))

    (compile "make" t)
    (with-current-buffer buf
      (add-hook 'compilation-finish-functions on-complete nil t))))

(defun config-nixos-set-stable ()
  "Build the NixOS environment."
  (interactive)
  (let* ((default-directory "~/Sync/nix")
         (compilation-buffer-name-function (-const config-nixos--compile-bufname))

         (compilation-process-setup-function
          (lambda ()
            (with-current-buffer (get-buffer-create config-nixos--compile-bufname)
              (when-let* ((proc (get-buffer-process (current-buffer))))
                (awesomewm-notify "NixOS build" "Starting build...")))))

         (on-complete
          (lambda (buf msg)
            (with-current-buffer buf
              (pcase (string-trim msg)
                ((and "finished" (guard (string-match-p (rx bol "make: Nothing to be done for 'install'") (buffer-string))))
                 (awesomewm-notify "NixOS build" "No changes made to system."))
                ("finished"
                 (awesomewm-notify "NixOS build" "Marked system as stable."))
                (_
                 (awesomewm-notify-error "NixOS build" "Aborted with errors."))))))

         (buf (get-buffer-create config-nixos--compile-bufname)))

    (compile "make stable" t)
    (with-current-buffer buf
      (add-hook 'compilation-finish-functions on-complete nil t))))


(add-to-list 'display-buffer-alist
             `(,(rx bos "*nixos-build*" eos)
               (display-buffer-reuse-window
                display-buffer-fullframe)
               (reusable-frames . visible)))


;; Show a desktop notification when Emacs wants you to enter your password.

(defun config-nixos--notify-on-ask-password (&rest _)
  (awesomewm-notify "Password required" "Emacs is waiting on password entry."))

(advice-add #'read-passwd :before #'config-nixos--notify-on-ask-password)

(provide 'config-nixos)

;;; config-nixos.el ends here
