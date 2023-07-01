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

(define-module (status network)
  #:use-module (ip addr)
  #:use-module (ip link)
  #:use-module (srfi srfi-1)
  #:export (carrier? link-speed routable? get-ip-addresses))

;; TODO: Use Netlink instead of sysfs..!
(define (read-network-status-file interface file)
  (let ((file-name (string-append "/sys/class/net/" interface "/" file)))
    (if (file-exists? file-name)
        (call-with-input-file file-name read)
        #f)))

(define (carrier? interface)
  (= 1 (read-network-status-file interface "carrier")))

(define (link-speed interface)
  (read-network-status-file interface "speed"))

(define (link-local-address? address)
  (or (string-prefix? "169.254" address)
      (string-prefix? "fe80:" address)))

(define (routable? address)
  (not (link-local-address? address)))

;; Return an alist of
;;    '("interface1" . '((ipv4 . '(addr1 addr2)) (ipv6 . '()))) pairs.
;; This is expensive as it does two netlink requests per invocation.
;; TODO: Cache?  Pipeline calls?  Subscribe to netlink events?
(define (get-ip-addresses)
  (let ((addresses (get-addrs))
        (links (get-links)))
    (let loop ((counter (length links))
               (interfaces '()))
      (if (= counter 0)
          interfaces
          (let* ((link (list-ref links (- counter 1)))
                 (link-addrs (filter
                              (lambda (address)
                                (equal? (addr-link address)
                                        (link-id link)))
                              addresses))
                 (ipv4 (filter-map (lambda (address)
                                     (if (equal? (addr-family address)
                                                 AF_INET)
                                         (addr-addr address)
                                         #f))
                                   link-addrs))
                 (ipv6 (filter-map (lambda (address)
                                     (if (equal? (addr-family address)
                                                 AF_INET6)
                                         (addr-addr address)
                                         #f))
                                   link-addrs)))
            (loop (- counter 1)
                  (alist-cons (link-name link)
                              `((ipv4 . ,ipv4)
                                (ipv6 . ,ipv6))
                              interfaces)))))))
