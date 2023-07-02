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

(define-module (status collector cpu-temperature)
  #:use-module (status)
  #:use-module (ice-9 format)
  #:export (cpu-temperature-status format-cpu-temperature-status))

(define (get-temperature file)
  (call-with-input-file file read))

(define* (cpu-temperature-status
          #:optional (temperature-file "/sys/class/thermal/thermal_zone0/temp")
          #:key (threshold 75))
  (let* ((temp (get-temperature temperature-file))
         (celsius (round (/ temp 1000))))
    (make-status
     "🌡"
     (if (> celsius threshold) 'bad 'neutral)
     celsius
     format-cpu-temperature-status)))

(define (format-cpu-temperature-status status)
  (format #f "~a ~d°C"
          (status-title status)
          (status-data status)))
