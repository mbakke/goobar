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

(define-module (status collector ethernet)
  #:use-module (status)
  #:use-module (status network)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (ethernet-status format-ethernet-status))

(define (ethernet-status interface)
  (let* ((carrier? (carrier? interface))
         (speed (link-speed interface))
         (ips (assoc-ref (get-ip-addresses) interface))
         (ipv4 (or (assoc-ref ips 'ipv4) '()))
         (ipv6 (or (assoc-ref ips 'ipv6) '()))
         ;; For convenience, add an "IP" key containing the first routable
         ;; IP address, which is what users want in most cases.
         (ip (find routable? ipv4)))
    (make-status
     "E:"
     (if carrier? (if ip 'good 'degraded) 'bad)
     `((interface . ,interface)
       (carrier? . ,carrier?)
       (speed . ,speed)
       (ipv4 . ,ipv4)
       (ipv6 . ,ipv6)
       (ip . ,ip))
     format-ethernet-status)))

(define (format-ethernet-status status)
  (let* ((icon (status-title status))
         (data (status-data status))
         (carrier? (assq-ref data 'carrier?)))
    (if carrier?
        (format #f "~a ~a~a"
                icon
                (or (assoc-ref data 'ip) "up")
                (format #f "~@[ (~d Mbit/s)~]" (assoc-ref data 'speed)))
        (format #f "~a down" icon))))
