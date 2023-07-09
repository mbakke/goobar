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
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (goobar-output-type
            goobar-output-head
            goobar-output-body
            goobar-output-tail
            goobar-output-foot
            get-goobar-output))

(define-record-type <goobar-output>
  (make-goobar-output type head body tail foot)
  goobar-output?
  ;; Symbol, currently either 'term or 'i3bar.
  (type goobar-output-type)
  ;; String printed at program startup.
  (head goobar-output-head)
  ;; Procedure to run every loop.
  (body goobar-output-body)
  ;; String printed before next loop (after sleeping).
  (tail goobar-output-tail)
  ;; String to print on exit.
  (foot goobar-output-foot))

(define* (status-list->terminal-output status-list #:key (separator "|"))
  (format #t "~a~%"
          (string-join (map (compose element->ansi-colored-string status->element)
                            status-list)
                       separator)))

(define (status-list->json-output status-list)
  (let ((statusen (map (compose element->json status->element) status-list)))
    (format #t "[~a]~%" (string-join statusen ","))))

(define (get-goobar-output output)
  (match output
    ('term
     (make-goobar-output 'term #f status-list->terminal-output #f #f))
    ('i3bar
     (make-goobar-output 'i3bar
                         "{\"version\":1}\n[\n"
                         status-list->json-output
                         ","
                         "]\n"))))
