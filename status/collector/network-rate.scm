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

(define-module (status collector network-rate)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (status)
  #:export (network-rate-status format-bytes))

(define-record-type <interface-cache>
  (make-interface-cache timestamp interfaces)
  interface-cache?
  (timestamp interface-cache-timestamp)
  (interfaces interface-cache-interfaces))

;; This variable holds the statistics of all interfaces.  Use a timestamp
;; to prevent updating multiple times in the same second.
;; TODO: It would be cleaner to pass the result of the previous run as
;; argument to the status collector somehow..!
(define %interfaces (make-interface-cache #f '()))

(define-record-type <interface-statistics>
  (make-interface-statistics receive transmit)
  interface-statistics?
  (receive interface-statistics-receive)
  (transmit interface-statistics-transmit))

(define (get-network-statistics)
  (let ((data (call-with-input-file "/proc/net/dev" get-string-all)))
    (match (string-split data #\newline)
      ((_ fields interfaces ...)
       (match-let (((_ rx-fields tx-fields) (string-split fields #\|)))
         (let ((rx-counters (string-tokenize rx-fields))
               (tx-counters (string-tokenize tx-fields)))
           (let loop ((count (length interfaces))
                      (stats '()))
             (if (= count 0)
                 stats
                 (let ((tokens (string-tokenize (list-ref interfaces (- count 1)))))
                   (match tokens
                     (() (loop (- count 1) stats))
                     ((ifname counters ...)
                      (let ((numeric-counters (map string->number counters)))
                        (loop (- count 1)
                              (alist-cons (string-drop-right ifname 1)
                                          (make-interface-statistics
                                           (zip rx-counters numeric-counters)
                                           (zip tx-counters
                                                (list-tail numeric-counters
                                                           (length rx-counters))))
                                          stats))))))))))))))

(define (calculate-rate new old interval)
  (* 1e9 (/ (- new old) interval)))

(define* (network-rate-status interface #:key (format format-network-rate))
  (let* ((now (current-time 'time-monotonic))
         (cached-timestamp (interface-cache-timestamp %interfaces))
         (cached-stats (interface-cache-interfaces %interfaces)))
    (if cached-timestamp
        (let ((cached-if (assoc-ref cached-stats interface))
              (current-stats (get-network-statistics)))
          (if cached-if
              (let* ((current-if (assoc-ref current-stats interface))
                     (age (time-difference now cached-timestamp))
                     (ns (+ (* 1e9 (time-second age))
                            (time-nanosecond age))))
                ;; We could assert that the pairs are indeed (bytes N),
                ;; (packets N), etc, but it is probably safe to assume that
                ;; this format does not change much.
                (match-let ((((_ crx-bytes) (_ crx-packets) (_ crx-errs) rest ...)
                             (interface-statistics-receive cached-if))
                            (((_ ctx-bytes) (_ ctx-packets) (_ ctx-errs) rest ...)
                             (interface-statistics-transmit cached-if))
                            (((_ rx-bytes) (_ rx-packets) (_ rx-errs) rest ...)
                             (interface-statistics-receive current-if))
                            (((_ tx-bytes) (_ tx-packets) (_ tx-errs) rest ...)
                             (interface-statistics-transmit current-if)))
                  ;; Update the cache.
                  (set! %interfaces (make-interface-cache now current-stats))
                  (make-status
                   interface 'neutral
                   `((rx-bytes/sec . ,(calculate-rate rx-bytes crx-bytes ns))
                     (tx-bytes/sec . ,(calculate-rate tx-bytes ctx-bytes ns)))
                   format)))
              ;; Hmm, interface not in cache?  Probably does not exist.
              (make-status interface 'bad #f format-interface-not-found)))
        (begin
          ;; First run..!
          (set! %interfaces (make-interface-cache now (get-network-statistics)))
          (make-status interface 'degraded #f format-no-data)))))

(define (format-bytes bytes)
  ;; XXX: Consider using a consistent output length to avoid resizing.
  (cond ((> bytes 1e9)
         (format #f "~1,2f GiB" (/ bytes 1024 1024 1024)))
        ((> bytes 1e6)
         (format #f "~1,2f MiB" (/ bytes 1024 1024)))
        ((> bytes 1e3)
         (format #f "~1,2f KiB" (/ bytes 1024)))
        ((= bytes 0) "0 B")
        (else (format #f "~1,2f B" bytes))))

(define (format-interface-not-found status)
  (format #f "~a not found" (status-title status)))

(define (format-no-data status)
  (format #f "~a: <no data>" (status-title status)))

(define (format-network-rate status)
  (let ((data (status-data status)))
    (format #f "⬇️~a/s ⬆️~a/s"
            (format-bytes (assoc-ref data 'rx-bytes/sec))
            (format-bytes (assoc-ref data 'tx-bytes/sec)))))
