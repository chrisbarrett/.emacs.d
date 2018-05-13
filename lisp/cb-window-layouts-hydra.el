;;; cb-window-layouts-hydra.el --- Hydras for eyebrowse  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'eyebrowse)
(require 'hydra)
(require 'dash)
(require 'subr-x)

(autoload 'cb-projectile-eyebrowse-switch-to-project "cb-projectile-eyebrowse")

(defun cb-window-layouts-hydra--render-name (name slot-number)
  (cond
   ((null name)
    (propertize "---" 'face 'shadow))
   ((string-blank-p name)
    (format "slot %s" (1+ slot-number)))
   (t
    (propertize name ))))

(defun cb-window-layouts-hydra--slot-indicator (slot-number)
  (-let* ((current-slot (eyebrowse--get 'current-slot))
          (active? (eq slot-number current-slot))
          ((&alist slot-number (_ name)) (eyebrowse--get 'window-configs)))
    (propertize (cb-window-layouts-hydra--render-name name slot-number) 'face (if active? '(underline t) 'shadow))))

(defun cb-window-layouts-hydra--current-layout ()
  (-let* ((current-slot (eyebrowse--get 'current-slot))
          ((&alist current-slot (_ name)) (eyebrowse--get 'window-configs)))
    (cb-window-layouts-hydra--render-name name current-slot)))

(defhydra cb-window-layouts (:foreign-keys warn :hint nil)
  "
Current layout: %s(propertize (cb-window-layouts-hydra--current-layout) 'face 'bold)

^Actions^               ^^   Layouts   ^^
^-------^---------------^^-- ----------^^-----------------------
_n_: next    _p_: previous   1. _e_: ?e?
_r_: rename  _x_: clear      2. _u_: ?u?
_s_: switch  _SPC_: toggle   3. _o_: ?o?
_RET_: done ^^               4. _a_: ?a?
"
  ("e" eyebrowse-switch-to-window-config-0 (cb-window-layouts-hydra--slot-indicator 0))
  ("u" eyebrowse-switch-to-window-config-1 (cb-window-layouts-hydra--slot-indicator 1))
  ("o" eyebrowse-switch-to-window-config-2 (cb-window-layouts-hydra--slot-indicator 2))
  ("a" eyebrowse-switch-to-window-config-3 (cb-window-layouts-hydra--slot-indicator 3))
  ("SPC" eyebrowse-last-window-config :exit t)
  ("n" eyebrowse-next-window-config)
  ("p" eyebrowse-prev-window-config)
  ("j" eyebrowse-next-window-config)
  ("k" eyebrowse-prev-window-config)
  ("N" eyebrowse-prev-window-config)
  ("r" eyebrowse-rename-window-config)
  ("x" eyebrowse-close-window-config)
  ("s" cb-projectile-eyebrowse-switch-to-project :exit t)
  ("RET" hydra-keyboard-quit :exit t)
  ("q" hydra-keyboard-quit :exit t))

(provide 'cb-window-layouts-hydra)

;;; cb-window-layouts-hydra.el ends here
