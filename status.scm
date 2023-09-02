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

(define-module (status)
  #:use-module (srfi srfi-9)
  #:export (make-status
            status?
            status-title status-state status-data status-printer status-short
            status->string
            status->short-string
            status-good?
            status-degraded?
            status-bad?
            status-critical?
            status-neutral?))

(define-record-type <status>
  (%make-status title state data printer short)
  status?
  (title status-title)                  ;symbol
  (state status-state)                  ;symbol
  (data status-data)                    ;any data type
  (printer status-printer)              ;procedure
  (short status-short))                 ;#f | procedure

(define* (make-status title state data printer #:optional short)
  (%make-status title state data printer
                (or short (const #f))))

(define (status->string status)
  ((status-printer status) status))

(define (status->short-string status)
  ((status-short status) status))

(define (status-good? status)
  (eq? 'good (status-state status)))

(define (status-degraded? status)
  (eq? 'degraded (status-state status)))

(define (status-bad? status)
  (eq? 'bad (status-state status)))

(define (status-critical? status)
  (eq? 'critical (status-state status)))

(define (status-neutral? status)
  (eq? 'neutral (status-state status)))
