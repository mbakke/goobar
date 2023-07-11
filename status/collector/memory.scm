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

(define-module (status collector memory)
  #:use-module (status)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:export (memory-status))

(define (extract-numbers str)
  (string->number (last (string-tokenize str char-set:digit))))

(define (get-memory-status)
  (let ((meminfo (open-file "/proc/meminfo" "r")))
    (let loop ((items '()))
      (let ((line (read-line meminfo)))
        (if (eof-object? line)
            (begin
              (close-port meminfo)
              items)
            (cond
             ((string-prefix? "MemTotal:" line)
              (loop (cons `(memory-total . ,(extract-numbers line)) items)))
             ((string-prefix? "MemFree:" line)
              (loop (cons `(memory-free . ,(extract-numbers line)) items)))
             ((string-prefix? "MemAvailable:" line)
              (loop (cons `(memory-available . ,(extract-numbers line)) items)))
             ((string-prefix? "Buffers:" line)
              (loop (cons `(buffers . ,(extract-numbers line)) items)))
             ((string-prefix? "Cached:" line)
              (loop (cons `(cached . ,(extract-numbers line)) items)))
             ((string-prefix? "Shmem:" line)
              (loop (cons `(shared-memory . ,(extract-numbers line)) items)))
             (else (loop items))))))))

(define* (memory-status #:key
                        (format format-memory-status)
                        (low-threshold (* 512 1024)))
  (let* ((status (get-memory-status))
         (memory-used (- (assoc-ref status 'memory-total)
                         (assoc-ref status 'memory-available))))
    (make-status
     'memory
     (cond ((< (assoc-ref status 'memory-available) low-threshold) 'bad)
           (else 'neutral))
     `((memory-used . ,memory-used) ,@status)
     format)))

;; TODO: Better representation and defaults + calculate percentages.
(define (format-memory-status status)
  (let ((data (status-data status)))
    (format #f "~1,1f GiB | ~1,1f GiB"
            (/ (assoc-ref data 'memory-used) 1024 1024)
            (/ (assoc-ref data 'memory-available) 1024 1024))))
