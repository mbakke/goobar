;;; This file is part of Goobar.
;;;
;;; Copyright © 2023 Marius Bakke
;;;
;;; Goobar is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Goobar is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Goobar. If not, see <https://www.gnu.org/licenses/>.

(define-module (goobar options)
  #:export (%options display-help-and-exit))

;; TODO: Print better error messages when validation fails.
(define (valid-interval? opt)
  (and (string-every char-set:digit opt)
       (positive? (string->number opt))))

(define (valid-output? opt)
  (member opt '("term" "i3bar")))

(define %options
  `((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (config-file (single-char #\c)
                 (value #t)
                 (predicate ,file-exists?))
    (interval (single-char #\i)
              (value #t)
              (predicate ,valid-interval?))
    ;; TODO: Why is -1 not working?
    (one-shot (value #f)) ;(single-char #\1)
    (output-format (single-char #\o)
                   (value #t)
                   (predicate ,valid-output?))
    (pid-file (single-char #\p)
              (value optional))))

(define (display-help-and-exit)
  (display "\
Usage: goobar [options]
  -h, --help             Show this text.
  -c, --config-file      Use this configuration file instead of the default.
  -i, --interval         Seconds to wait between iterations (default 5).
  -o, --output-format    Which output format to use.  Either 'term' or 'i3bar'.
  -p, --pid-file[=FILE]  Save the PID to FILE.  If FILE is omitted, the
                         PID is stored in $XDG_RUNTIME_DIR/goobar/pid.
  --one-shot             Run once and exit instead of looping.
")
  (exit 0))
