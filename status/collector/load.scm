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
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (load-status format-load-status))

(define (load-status)
  (match (string-split (call-with-input-file "/proc/loadavg" get-string-all) #\ )
    ((1m 5m 15m rest ...)
     `((icon . "🏋")
       (1min . ,1m)
       (5min . ,5m)
       (15min . ,15m)))))

(define* (format-load-status status #:optional (period '1min))
  (format #f "~a ~a"
          (assoc-ref status 'icon)
          (assoc-ref status period)))
