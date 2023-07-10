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

(define-module (goobar colors)
  #:use-module (ice-9 match)
  #:export (state->color ansi-paint))

(define (state->color state)
  (match state
    ('good "#00FF00")
    ('degraded "#FFD000")
    ('bad "#FF0000")
    (_ #f)))

(define (ansi-paint str color)
  ;; TODO: How widely supported is this?  We probably don't need the
  ;; full 24-bit color range for terminals.  Maybe round it down to the
  ;; nearest 256 color palette like i3status or tmux (I just could not
  ;; figure out their algorithms..).
  (let ((r (string->number (substring color 1 3) 16))
        (g (string->number (substring color 3 5) 16))
        (b (string->number (substring color 5 7) 16)))
    (string-append (string #\esc #\[)
                   (format #f "38;2;~d;~d;~dm" r g b)
                   str
                   (string #\esc #\[) "0m")))
