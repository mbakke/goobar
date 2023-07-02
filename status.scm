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

(define-module (status)
  #:use-module (srfi srfi-9)
  #:export (make-status
            status?
            status-title status-status status-data status-printer
            good?
            degraded?
            bad?))

;; TODO: Support default values and inheritance à la (guix records)?
(define-record-type <status>
  (make-status title status data printer)
  status?
  (title status-title)                  ;<string> (often emoji..)
  (status status-status)                ;'good|'degraded|'bad
  (data status-data)                    ;any data type
  (printer status-printer))             ;procedure

(define (good? status)
  (eq? 'good (status-status status)))

(define (degraded? status)
  (eq? 'degraded (status-status status)))

(define (bad? status)
  (eq? 'bad (status-status status)))
