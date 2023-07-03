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

(define-module (status collector disk)
  #:use-module (status)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (goobar external)
  #:export (disk-status format-disk-status))

;; TODO: Get rid of DF!
(define (read-df mount-point)
  (let* ((input (open-input-pipe (string-append %df
                                                " -h --output=used,avail,pcent "
                                                mount-point)))
         (data (get-string-all input)))
    (close-port input)
    ;; Drop the first line of data (header).
    (let ((lines (string-split data #\newline)))
      ;; The output is " USED  AVAIL  PCENT"
      (string-join (cdr lines) "\n"))))

(define (percentage->number threshold)
  (string->number (string-drop-right threshold 1)))

(define* (disk-status mount-point
                      #:key
                      (bad-threshold "95%")
                      (degraded-threshold "90%"))
  (let ((bad (percentage->number bad-threshold))
        (degraded (percentage->number degraded-threshold)))
    (match (string-tokenize (read-df mount-point))
      ((used avail pcent)
       ;; TODO: Implement K/M/G/T conversion and thresholds.
       (let ((percent-used (percentage->number pcent)))
         (make-status
          "🖴"
          (cond ((> percent-used bad) 'bad)
                ((> percent-used degraded) 'degraded)
                (else 'neutral))
          `((mount-point . ,mount-point)
            (used . ,used)
            (available . ,avail)
            (percent-used . ,percent-used))
          format-disk-status))))))

(define (format-disk-status status)
  (let ((data (status-data status)))
    (format #f "~a ~d%"
            (status-title status)
            (assoc-ref data 'percent-used))))
