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
                   (name (match obj
                           ((? status?) (status-title obj))
                           (_ #f)))
                   (instance #f)
                   (full-text (match obj
                                ((? status?) (status->string obj))
                                ((? string?) obj)
                                ;; TODO: Maybe error here..?
                                (_ #f)))
                   (short-text #f)
                   (color (match obj
                            ((? status?) (status->color (status-state obj)))
                            (_ #f)))
                   (background #f)
                   (border #f)
                   (border-top #f)
                   (border-bottom #f)
                   (border-left #f)
                   (border-right #f)
                   (min-width #f)
                   (align #f)
                   (urgent? #f)
                   (separator? #t)
                   (block-width #f)
                   (markup #f))
  (make-element name instance full-text short-text color background
                border border-top border-bottom border-left border-right
                min-width align urgent? separator? block-width markup))

(define (status->element status)
  (match status
    ((? status?) (annotate status))
    ((? string?) (annotate status #:name status #:full-text status))
    ((? element?) status)
    (_ (annotate (format #f "can not process ~a" status)
                 #:color (status->color 'bad)))))
