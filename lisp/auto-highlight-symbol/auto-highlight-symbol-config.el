;;; auto-highlight-symbol-config.el --- auto highlight symbol mode configuration file

;; Copyright (C) 2010 Mitsuo Saito
;; Created date 2010-10-30 15:24 +0900

;; Author: Mitsuo Saito <arch320@NOSPAM.gmail.com>
;; Keywords: face match convenience
;; URL: http://github.com/mitsuo-saito/auto-highlight-symbol-mode/raw/master/auto-highlight-symbol-config.el
;; Compatibility: GNU Emacs 22.3 23.x 24.x later
;;
;; This file is NOT part of GNU Emacs.

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
;;
;; Basic steps to setup:
;;   1. Place `auto-highlight-symbol-config.el' in your `load-path'.
;;   2. In your `.emacs.el' file
;;          (require 'auto-highlight-symbol-config)
;;

;;
;; Happy Coding !!
;;

;;; SCM Log
;;
;;   $Revision: 70:542ae42370e4 tip $
;;   $Commiter: Mitso Saito <arch320@NOSPAM.gmail.com> $
;;   $LastModified: Thu, 04 Nov 2010 12:52:08 +0900 $
;;
;;   $Lastlog: some tweak $
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (unless (fboundp 'auto-complete-mode)
    (defun auto-complete-mode(arg))))

(defconst auto-highlight-symbol-config-vers "$Id: auto-highlight-symbol-config.el,v 70:542ae42370e4 2010-11-04 12:52 +0900 arch320 $"
  "auto-highlight-symbol-config version")

;;
;; (@* "Require" )
;;
(require 'auto-highlight-symbol )

;;
;; (@* "Variable" )
;;

;;
;; (@* "Face" )
;;

;;
;; (@* "Plugins" )
;;

;;
;; (@* "Avoid old vers auto-complete menu" )
;;
(defvar ahs-ac-active-flag nil)
(make-variable-buffer-local 'ahs-ac-active-flag)

(defun ahs-avoid-auto-complete-menu ()
  "Avoid auto-complete-mode menu for protect overlay"
  (when (featurep 'auto-complete)
    (setq ahs-ac-active-flag
          (or ahs-ac-active-flag
              (assoc-default 'auto-complete-mode (buffer-local-variables))))
    (when ahs-ac-active-flag
      (auto-complete-mode 0))))

(defun ahs-recover-auto-complete ()
  "Recover auto-complete-mode"
  (when (and (featurep 'auto-complete)
             ahs-ac-active-flag)
    (auto-complete-mode t)
    (setq ahs-ac-active-flag nil)))

;;
;; if you use old vers auto-complete
;;  comment out for protect overlay in edit mode
;;
;;(add-hook 'ahs-edit-mode-on-hook  'ahs-avoid-auto-complete-menu)
;;(add-hook 'ahs-edit-mode-off-hook 'ahs-recover-auto-complete)
;;

;;
;; (@* "Fire up" )
;;
(unless global-auto-highlight-symbol-mode
  (global-auto-highlight-symbol-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auto-highlight-symbol-config)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:

;;
;; $Id: auto-highlight-symbol-config.el,v 70:542ae42370e4 2010-11-04 12:52 +0900 arch320 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-highlight-symbol-config.el ends here
