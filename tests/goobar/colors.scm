;;; This file is part of Goobar.
;;;
;;; Copyright © 2023 Marius Bakke
;;;
;;; Goobar is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Foobar is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Goobar. If not, see <https://www.gnu.org/licenses/>.

(define-module (test-colors)
  #:use-module (goobar colors)
  #:use-module (srfi srfi-64))

(test-begin "colors")

(test-equal "state->color"
  "#00FF00"
  (state->color 'good))

(test-equal "state->color, critical, first invocation"
  "#8B0000"
  (state->color 'critical))

(test-equal "state->color, critical, second invocation"
  "#FFA000"
  (state->color 'critical))

(test-equal "state->color, critical, third invocation"
  "#8B0000"
  (state->color 'critical))

(test-assert "validate color, valid color"
  (validate-color "#000000"))

(test-eq "validate color, invalid color, short"
  #f
  (validate-color "#ABC"))

(test-eq "validate color, invalid color, empty string"
  #f
  (validate-color ""))

(test-eq "validate color, invalid color, non-hex"
  #f
  (validate-color "#XXXXXX"))

(exit (zero? (test-runner-fail-count (test-end))))
