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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (%options display-help-and-exit))

;; TODO: Print better error messages when validation fails.
(define (valid-interval? opt)
  (and (string-every char-set:digit opt)
       (positive? (string->number opt))))

(define %supported-outputs
  '("term" "i3bar"))

(define (valid-output? opt)
  (member opt %supported-outputs))

(define %options
  `((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (config (single-char #\c)
            (value #t)
            (predicate ,file-exists?))
    (interval (single-char #\i)
              (value #t)
              (predicate ,valid-interval?))
    ;; TODO: Support -1 when <https://bugs.gnu.org/42669> is merged.
    (one-shot (value #f)) ;(single-char #\1)
    (output (single-char #\o)
            (value #t)
            (predicate ,valid-output?))
    (pid-file (single-char #\p)
              (value optional))))

(define (display-help-and-exit)
  (format #t  "\
Usage: goobar [options]
  -h, --help                Show this text.
  -c, --config=FILE         Use FILE instead of the default configuration.
  -i, --interval=INTERVAL   Wait INTERVAL seconds between iterations (default 5).
  -o, --output=FORMAT       Use FORMAT style output instead of guessing.
                            Supported values are: ~a.
  -p, --pid-file[=FILE]     Save the PID to FILE.  If FILE is omitted, the
                            PID is stored in $XDG_RUNTIME_DIR/goobar/pid.
  --one-shot                Run once and exit instead of looping.
"
          (match %supported-outputs
            ((outputs ... last)
             (string-append (string-join (map (lambda (output)
                                                (string-append "'" output "'"))
                                              outputs)
                                         ", ")
                            ", and '" last "'"))))
  (exit 0))
