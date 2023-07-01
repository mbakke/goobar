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
  #:use-module (status network)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (ethernet-status format-ethernet-status))

(define (ethernet-status interface)
  (let* ((carrier? (carrier? interface))
         (speed (link-speed interface))
         (ips (assoc-ref (get-ip-addresses) interface))
         (ipv4 (or (assoc-ref ips 'ipv4) '()))
         (ipv6 (or (assoc-ref ips 'ipv6) '())))
    `((interface . ,interface)
      (carrier? . ,carrier?)
      (speed . ,speed)
      (ipv4 . ,ipv4)
      (ipv6 . ,ipv6)
      ;; For convenience, add an "IP" key containing the first routable
      ;; IP address, which is what users want in most cases.
      (ip . ,(find routable? ipv4)))))

(define (format-ethernet-status status)
  (let ((carrier? (assq-ref status 'carrier?)))
    (if carrier?
        (format #f "E: ~a~a"
                (or (assoc-ref status 'ip) "up")
                (format #f "~@[ (~d Mbit/s)~]" (assoc-ref status 'speed)))
        (format #f "E: down"))))
