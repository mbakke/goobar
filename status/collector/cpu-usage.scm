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

(define-module (status collector cpu-usage)
  #:use-module (status)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:export (cpu-usage-status))

(define-record-type <cpu-usage>
  (make-cpu-usage user nice system idle total)
  cpu-usage?
  (user cpu-usage-user)
  (nice cpu-usage-nice)
  (system cpu-usage-system)
  (idle cpu-usage-idle)
  (total cpu-usage-total))

;; Variable to track CPU usage between iterations.
(define %cpu-usage (make-cpu-usage 0 0 0 0 0))

(define (get-cpu-usage)
  (let ((stat (call-with-input-file "/proc/stat" get-line)))
    (match (map string->number (string-tokenize stat char-set:digit))
      ((user nice system idle rest ...)
       (make-cpu-usage user nice system idle
                       (+ user nice system idle))))))

(define* (cpu-usage-status #:key
                           (format format-cpu-usage-status)
                           (bad-threshold 95)
                           (degraded-threshold 90))
  (let* ((previous %cpu-usage)
         (current (get-cpu-usage))
         (diff-idle (- (cpu-usage-idle current)
                       (cpu-usage-idle previous)))
         (diff-total (- (cpu-usage-total current)
                        (cpu-usage-total previous)))
         (diff-usage (round (* 100 (- 1 (/ diff-idle diff-total))))))
    (set! %cpu-usage current)
    (make-status
     'cpu-usage
     (cond ((> diff-usage bad-threshold) 'bad)
           ((> diff-usage degraded-threshold) 'degraded)
           (else 'neutral))
     diff-usage format)))

(define (format-cpu-usage-status status)
  (format #f "~2,'0d%" (status-data status)))
