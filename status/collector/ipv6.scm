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

(define-module (status collector ipv6)
  #:use-module (status)
  #:export (ipv6-status))

(define* (ipv6-status #:key (format format-ipv6-status))
  (with-exception-handler
      (lambda (err)
        ;; Probably no IPv6 connectivity.  Don't bother printing errors.
        (make-status 'ipv6 'bad #f format))
    (lambda ()
      ;; Attempt to establish a connection to the K root DNS server.  Use
      ;; IP address to avoid DNS lookup.
      (let* ((ai (car (getaddrinfo "2001:7fd::1" "domain"
                                   ;; Ensure no DNS lookup, and return only
                                   ;; locally available addresses.
                                   (logior AI_NUMERICHOST AI_ADDRCONFIG)
                                   AF_INET6
                                   ;; Use SOCK_DGRAM to prevent any actual
                                   ;; network traffic.
                                   SOCK_DGRAM)))
             (sock (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                           (addrinfo:protocol ai))))
        (connect sock (addrinfo:addr ai))
        (make-status 'ipv6 'good
                     (inet-ntop AF_INET6 (sockaddr:addr (getsockname sock)))
                     format)))
    #:unwind? #t))

(define (format-ipv6-status status)
  (format #f "~a"
          (if (status-good? status) (status-data status) "❌")))
