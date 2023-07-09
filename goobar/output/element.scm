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

(define-module (goobar output element)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (goobar colors)
  #:export (make-element
            element?
            element-name
            element-instance
            element-full-text
            element-short-text
            element-color
            element-background
            element-border
            element-border-top
            element-border-bottom
            element-border-left
            element-border-right
            element-min-width
            element-align
            element-urgent?
            element-separator?
            element-block-width
            element-markup

            element->ansi-colored-string
            element->json))

;; This is essentially the same fields as specified by swaybar-protocol(7).
(define-record-type <element>
  (make-element name instance full-text short-text color background
                border border-top border-bottom border-left border-right
                min-width align urgent? separator? block-width markup)
  element?
  (name element-name)
  (instance element-instance)
  (full-text element-full-text)
  (short-text element-short-text)
  (color element-color)
  (background element-background)
  (border element-border)
  (border-top element-border-top)
  (border-bottom element-border-bottom)
  (border-left element-border-left)
  (border-right element-border-right)
  (min-width element-min-width)
  (align element-align)
  (urgent? element-urgent?)
  (separator? element-separator?)
  (block-width element-separator-block-width)
  (markup element-markup))

(define (element->ansi-colored-string element)
  (let ((color (element-color element))
        (text (element-full-text element)))
    (if color
        (string-append (hex->ansi-truecolor color)
                       text
                       (string #\esc #\[) "0m")
        text)))

(define (element->json element)
  (format #f "{\"full_text\":\"~a\"~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a~a}"
          (element-full-text element)
          (format #f "~@[,\"short_text\":\"~a\"~]" (element-short-text element))
          (format #f "~@[,\"color\":\"~a\"~]" (element-color element))
          (format #f "~@[,\"background\":\"~a\"~]" (element-background element))
          (format #f "~@[,\"border\":\"~a\"~]" (element-border element))
          (format #f "~@[,\"border_top\":\"~d\"~]" (element-border-top element))
          (format #f "~@[,\"border_bottom\":\"~d\"~]" (element-border-bottom element))
          (format #f "~@[,\"border_left\":\"~d\"~]" (element-border-left element))
          (format #f "~@[,\"border_right\":\"~d\"~]" (element-border-right element))
          (format #f "~@[,\"min_width\":\"~a\"~]" (element-min-width element))
          (format #f "~@[,\"align\":\"~a\"~]" (element-align element))
          (format #f "~@[,\"name\":\"~a\"~]" (element-name element))
          (format #f "~@[,\"instance\":\"~a\"~]" (element-instance element))
          (format #f "~@[,\"urgent\":\"~a\"~]"
                  (if (element-urgent? element) "true" #f))
          (format #f "~@[,\"separator\":\"~a\"~]"
                  (if (element-separator? element) #f "false"))
          (format #f "~@[,\"separator_block_width\":\"~d\"~]"
                  (element-separator-block-width element))
          (format #f "~@[,\"markup\":\"~a\"~]" (element-markup element))))
