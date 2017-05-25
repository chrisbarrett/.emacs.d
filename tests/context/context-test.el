;;; context-test.el --- tests for ConTeXt mode

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(require 'ert)
(require 'context)

(defun AUCTeX-set-ert-path (&rest sym-val)
  "Set first element of SYM-VAL to the next one, and so on.

The value is the path to the test file, make sure it is expanded
in the right directory even when the ERT test from the command
line and from another directory."
  (while sym-val
    (set (pop sym-val)
	 (expand-file-name (pop sym-val)
			   (when load-file-name
			     (file-name-directory load-file-name))))))

(AUCTeX-set-ert-path
 'ConTeXt-indent-test/in
 "context-indentation-in.tex"
 'ConTeXt-indent-test/out
 "context-indentation-out.tex")

(ert-deftest ConTeXt-indent ()
  (should (string=
           (with-temp-buffer
             (insert-file-contents ConTeXt-indent-test/in)
             (ConTeXt-mode)
             (indent-region (point-min) (point-max))
             (buffer-string))
           (with-temp-buffer
             (insert-file-contents ConTeXt-indent-test/out)
             (buffer-string)))))

;;; context-test.el ends here
