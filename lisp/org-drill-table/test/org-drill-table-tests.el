;;; org-drill-table-tests.el --- Tests for org-drill-table.el

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Tests for org-drill-table.el

;;; Code:

(require 'ert)
(require 'org-drill-table)

(defun org-drill-table--setup-buffer (str)
  (org-mode)
  (goto-char (point-min))
  (save-excursion
    (insert str)))

(ert-deftest org-drill-table--creates-cards-heading ()
  (with-temp-buffer
    (org-drill-table--setup-buffer "

* Test Heading
| X  | Y  |
|----+----|
| X1 | Y1 |
| X2 | Y2 |
| X3 | Y3 |

")
    (search-forward "Test Heading")
    (org-drill-table-generate "_" "_" "_")
    (should (s-match (rx (+ "*") (+ space) "Cards")
                     (buffer-string)))))

(ert-deftest org-drill-table--error-if-no-table-at-pt ()
  (with-temp-buffer
    (org-drill-table--setup-buffer "")
    (should-error (org-drill-table-generate "_" "_" "_"))))

(ert-deftest org-drill-table--tracks-different-subheading-separately ()
  (with-temp-buffer
    (org-drill-table--setup-buffer "

* L1
** L2a
| X  | Y  |
|----+----|
| X1 | Y1 |
** L2b
| X  | Y  |
|----+----|
| X2 | Y2 |
** L2c
| X  | Y  |
|----+----|
| X3 | Y3 |

")
    (dolist (heading '("L2a" "L2b" "L2c"))
      (search-forward heading)
      (org-drill-table-generate heading "ty" "_"))

    (should (equal (buffer-string) "

* L1
** L2a
| X  | Y  |
|----+----|
| X1 | Y1 |
*** Cards                                                          :noexport:
**** L2a                                                              :drill:
     :PROPERTIES:
     :DRILL_CARD_TYPE: ty
     :END:
_
***** X
X1
***** Y
Y1
** L2b
| X  | Y  |
|----+----|
| X2 | Y2 |
*** Cards                                                          :noexport:
**** L2b                                                              :drill:
     :PROPERTIES:
     :DRILL_CARD_TYPE: ty
     :END:
_
***** X
X2
***** Y
Y2
** L2c
| X  | Y  |
|----+----|
| X3 | Y3 |

*** Cards                                                          :noexport:

**** L2c                                                              :drill:
     :PROPERTIES:
     :DRILL_CARD_TYPE: ty
     :END:
_

***** X
X3

***** Y
Y3
"))))

(provide 'org-drill-table-tests)

;;; org-drill-table-tests.el ends here
