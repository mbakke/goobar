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

(define-module (test-annotation)
  #:use-module (status)
  #:use-module (goobar annotation)
  #:use-module (goobar output element)
  #:use-module (srfi srfi-64))

(test-begin "annotation")

(test-equal "backlight icon, 100%"
  "🔆"
  (string-take (element-full-text
                (annotate (make-status 'backlight 'neutral 100 (const ""))))
               1))

(test-equal "backlight icon, 1%"
  "🔅"
  (string-take (element-full-text
                (annotate (make-status 'backlight 'neutral 1 (const ""))))
               1))

(test-equal "backlight icon, not found"
  "no icon"
  (element-full-text (annotate (make-status 'backlight 'bad #f
                                            (const "no icon")))))

(test-equal "battery icon"
  "🔋"
  (string-take (element-full-text
                (annotate (make-status 'battery
                                       'neutral
                                       '((status . discharging))
                                       (const ""))))
               1))

(test-equal "battery icon, unknown status"
  "🦇"
  (string-take (element-full-text
                (annotate (make-status 'battery
                                       'neutral
                                       '((status . foo))
                                       (const ""))))
               1))

(test-equal "pulseaudio icon"
  "🔊"
  (string-take (element-full-text
                (annotate (make-status 'pulseaudio 'neutral
                                       '((mute? . #f))
                                       (const ""))))
               1))

(test-equal "pulseaudio icon, muted"
  "🔇"
  (string-take (element-full-text
                (annotate (make-status 'pulseaudio 'neutral
                                       '((mute? . #t))
                                       (const ""))))
               1))

(exit (zero? (test-runner-fail-count (test-end))))
