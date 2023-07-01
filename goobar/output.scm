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

(define-module (goobar output)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (status-printer-head
            status-printer-body
            status-printer-tail
            status-printer-foot
            get-status-printer))

(define-record-type <status-printer>
  (make-status-printer head body tail foot)
  status-printer?
  ;; String printed at program startup.
  (head status-printer-head)
  ;; Procedure to run every loop.
  (body status-printer-body)
  ;; String printed before next loop (after sleeping).
  (tail status-printer-tail)
  ;; String to print on exit.
  (foot status-printer-foot))

(define* (status-list->terminal-output status-list #:key (separator "|"))
  (format #t "~a~%" (string-join status-list separator)))

(define (status->json status)
  (format #f "{\"full_text\":\"~a\"}" status))

(define (status-list->json-output status-list)
  (let ((statusen (map status->json status-list)))
    (format #t "[~a]~%" (string-join statusen ","))))

(define (get-status-printer output)
  (match output
    ('term (make-status-printer
            #f
            status-list->terminal-output
            #f
            #f))
    ('i3bar (make-status-printer
             "{\"version\":1}\n[\n"
             status-list->json-output
             ","
             "]\n"))))
