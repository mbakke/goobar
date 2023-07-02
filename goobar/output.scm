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
  #:use-module (goobar colors)
  #:use-module (status)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (goobar-output-head
            goobar-output-body
            goobar-output-tail
            goobar-output-foot
            get-goobar-output))

(define-record-type <goobar-output>
  (make-goobar-output head body tail foot)
  goobar-output?
  ;; String printed at program startup.
  (head goobar-output-head)
  ;; Procedure to run every loop.
  (body goobar-output-body)
  ;; String printed before next loop (after sleeping).
  (tail goobar-output-tail)
  ;; String to print on exit.
  (foot goobar-output-foot))

(define (status->colored-string status)
  (match status
    ((? status?)
     (match (status->color status)
       (#f (colorize status))
       (_ (colorize (status-text status) (status->color status)))))
    ((? string?) (colorize status))
    ((? colored-string?) status)
    (_ (format #f "can not process ~a" status))))

(define (ansi-colorize colored-string)
  (let ((color (colored-string-color colored-string))
        (text (colored-string-string colored-string)))
    (if color
        (string-append (hex->ansi-truecolor color)
                       text
                       (string #\esc #\[) "0m")
        text)))

(define* (status-list->terminal-output status-list #:key (separator "|"))
  (format #t "~a~%"
          (string-join (map (compose ansi-colorize status->colored-string)
                            status-list)
                       separator)))

(define (status->json status)
  (let ((colored (status->colored-string status)))
    (format #f "{\"full_text\":\"~a\"~a}"
            (colored-string-string colored)
            (format #f "~@[,\"color\":\"~a\"~]"
                    (colored-string-color colored)))))

(define (status-list->json-output status-list)
  (let ((statusen (map status->json status-list)))
    (format #t "[~a]~%" (string-join statusen ","))))

(define (get-goobar-output output)
  (match output
    ('term (make-goobar-output
            #f
            status-list->terminal-output
            #f
            #f))
    ('i3bar (make-goobar-output
             "{\"version\":1}\n[\n"
             status-list->json-output
             ","
             "]\n"))))
