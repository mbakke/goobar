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
  #:use-module (goobar output element)
  #:use-module (goobar annotation)
  #:use-module (goobar colors)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (output-type
            output-head
            output-body
            output-tail
            output-foot
            get-output))

(define-record-type <output>
  (make-output type head body tail foot)
  output?
  ;; Symbol, currently either 'term or 'i3bar.
  (type output-type)
  ;; String printed at program startup.
  (head output-head)
  ;; Procedure to run every loop.
  (body output-body)
  ;; String printed before next loop (after sleeping).
  (tail output-tail)
  ;; String to print on exit.
  (foot output-foot))

(define* (status-list->terminal-output status-list #:key (separator " | "))
  (let ((colored-separator (ansi-paint separator "#646464")))
    (match (map status->element status-list)
      ((elements ... last)
       (for-each (lambda (element)
                   (format #t "~a~a"
                           (element->text element)
                           (if (element-separator? element)
                               colored-separator
                               " ")))
                 elements)
       (format #t "~a~%" (element->text last))))))

(define (status-list->json-output status-list)
  (let ((statusen (map (compose element->json status->element) status-list)))
    (format #t "[~a]~%" (string-join statusen ","))))

(define (get-output output)
  (match output
    ('term
     (make-output 'term #f status-list->terminal-output #f #f))
    ('i3bar
     (make-output 'i3bar "{\"version\":1}\n[\n"
                  status-list->json-output "," "]\n"))))
