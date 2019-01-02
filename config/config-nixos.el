;;; config-nixos.el --- NixOS-specific configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'general)

(general-define-key "s-c" #'clipboard-kill-ring-save)
(general-define-key "s-v" #'clipboard-yank)
(general-define-key "<s-return>" #'toggle-frame-fullscreen)

(provide 'config-nixos)

;;; config-nixos.el ends here
