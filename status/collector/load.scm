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

(define-module (status collector load)
  #:use-module (status)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module ((ice-9 threads) #:select (total-processor-count))
  #:export (load-status format-load-status))

(define* (load-status #:optional (period '1min)
                      #:key (threshold
                             ;; Paint red when above this threshold.
                             (let ((cpu-count (total-processor-count)))
                               (if (= 1 cpu-count)
                                   1
                                   (- cpu-count 1)))))
  (match (string-split (call-with-input-file "/proc/loadavg" get-string-all) #\ )
    ((1m 5m 15m rest ...)
     (let* ((data `((1min . ,1m) (5min . ,5m) (15min . ,15m)))
            (load (string->number (assoc-ref data period))))
       (make-status
        "🏋"
        (if (> load threshold) 'bad 'neutral)
        load
        format-load-status)))))

(define* (format-load-status status)
  ;; Format "~1,2f" correctly gives "1.10" for 1.1, but ".10" for 0.1,
  ;; which causes needless horizontal shift in the status bar when going
  ;; from <1 to >=1 or the oppositve.  Work around by using "monetary" style.
  ;; (XXX: Is it possible to prevent ~f from eliding the 0?)
  (format #f "~a ~$"
          (status-title status)
          (status-data status)))
