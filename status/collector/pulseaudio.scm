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

(define-module (status collector pulseaudio)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (goobar external)
  #:export (pulseaudio-status format-pulseaudio-status))

;; TODO: Can we do this without pactl..?
(define (muted? sink)
  (let* ((pipe (open-input-pipe (string-append %pactl " get-sink-mute "
                                               sink)))
         (result (get-string-all pipe)))
    (close-port pipe)
    (if (string-contains result "yes") #t #f)))

(define (volume sink)
  (let* ((pipe (open-input-pipe (string-append %pactl " get-sink-volume "
                                               sink)))
         (result (get-string-all pipe)))
    (close-port pipe)
    (match (string-split result #\/)
      ;; Brittle, but works...
      ((_ lpct _ rpct _)
       (if (string=? lpct rpct)
           lpct
           "???"))
      (_ "???"))))

(define (pulseaudio-status sink)
  (let* ((mute? (muted? sink))
         (icon (if mute? "🔇" "🔊")))
    `((icon . ,icon)
      (volume . ,(volume sink))
      (mute? . ,mute?))))

(define (format-pulseaudio-status status)
  (let ((icon (assoc-ref status 'icon))
        (mute? (assoc-ref status 'mute?))
        (volume (assoc-ref status 'volume)))
    (if mute?
        (format #f "~a (~a)" icon (string-trim-both volume))
        (format #f "~a ~a" icon (string-trim-both volume)))))
