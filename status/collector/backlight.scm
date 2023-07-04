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

(define-module (status collector backlight)
  #:use-module (status)
  #:use-module (ice-9 format)
  #:export (backlight-status format-backlight-status))

(define (read-backlight-file path file)
  (call-with-input-file (string-append path "/" file) read))

(define (backlight-status)
  ;; TODO: Support more drivers..!
  (let ((path "/sys/class/backlight/intel_backlight"))
    (if (file-exists? path)
        (let* ((brightness (read-backlight-file path "brightness"))
               (max (read-backlight-file path "max_brightness"))
               (percentage (round (* 100 (/ brightness max)))))
          (make-status (if (>= percentage 50) "🔆" "🔅")
                       'neutral
                       percentage
                       format-backlight-status))
        (make-status #f 'bad #f format-backlight-not-found))))

(define (format-backlight-not-found status)
  "<no backlight>")

(define (format-backlight-status status)
  (format #f "~a ~d%" (status-title status) (status-data status)))
