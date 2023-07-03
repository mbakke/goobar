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

(define-module (goobar options)
  #:export (%options display-help-and-exit))

(define (valid-interval? opt)
  (and (string-every char-set:digit opt)
       (positive? (string->number opt))))

(define (valid-output? opt)
  (member opt '("term" "i3bar")))

(define %options
  `((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (interval (single-char #\i)
              (value #t)
              (predicate ,valid-interval?))
    (output-format (single-char #\o)
                   (value #t)
                   (predicate ,valid-output?))))

(define (display-help-and-exit)
  (display "\
goobar [options]
  -h, --help           Show this text
  -i, --interval       Seconds to wait between iterations (default 5)
  -o, --output-format  Which output format to use.  Either 'term' or 'i3bar'.
")
  (exit 0))
