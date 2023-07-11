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

(define-module (goobar annotation)
  #:use-module (goobar colors)
  #:use-module (goobar output element)
  #:use-module (status)
  #:use-module (ice-9 match)
  #:export (annotate status->element))

(define* (annotate obj
                   #:key
                   (icon (match obj
                           ((? status?) (status->icon obj))
                           (_ #f)))
                   (name (match obj
                           ((? status?) (symbol->string (status-title obj)))
                           (_ #f)))
                   (instance #f)
                   (full-text (match obj
                                ((? status?) (status->string obj))
                                ((? string?) obj)
                                ;; TODO: Maybe error here..?
                                (_ #f)))
                   (short-text #f)
                   (color (match obj
                            ((? status?) (state->color (status-state obj)))
                            (_ #f)))
                   (background #f)
                   (border #f)
                   (border-top #f)
                   (border-bottom #f)
                   (border-left #f)
                   (border-right #f)
                   (min-width #f)
                   (align #f)
                   (urgent? (match obj
                              ((? status?) (status-critical? obj))
                              (_ #f)))
                   (separator? #t)
                   (block-width #f)
                   (markup #f))
  (make-element name instance
                (if icon
                    (string-append icon " " full-text)
                    full-text)
                short-text
                (validate-color color)
                background
                border border-top border-bottom border-left border-right
                min-width align urgent? separator? block-width markup))

(define (status->element status)
  (match status
    ((? status?) (annotate status))
    ((? string?) (annotate status #:name status #:full-text status))
    ((? element?) status)
    (_ (annotate (format #f "can not process ~a" status)
                 #:color (state->color 'bad)))))

(define (status->icon status)
  (let ((name (status-title status)))
    (match name
      ('backlight
       (match (status-data status)
         ((? integer? num) (if (> num 50)  "🔆" "🔅"))
         (_ #f)))
      ('battery
       (let ((data (status-data status)))
         (match (assoc-ref data 'status)
           ('discharging "🔋")
           ('charging "⚡")
           ('full "🔋☻")
           ('not-charging "🔌")
           ;; This should not happen.  Notify the user that something is off.
           (_  "🦇"))))
      ('cpu-usage "🔥")
      ('cpu-temperature "🌡")
      ('disk "🖴")
      ;; Why no RJ45 connector in Unicode spec :(
      ('ethernet "E:")
      ('ipv6 "IPv6")
      ('load "🏋")
      ;; Why no IC icon in Unicode?  🐏 or 🍪 is probably too abstract...
      ('memory "M:")
      ('pulseaudio
       (let ((data (status-data status)))
         (if (assoc-ref data 'mute?)
             "🔇" "🔊")))
      ('wifi "📶")
      (_ #f))))
