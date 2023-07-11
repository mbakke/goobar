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

(define-module (test-battery)
  #:use-module (status)
  #:use-module (status collector battery)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-64))

;;;
;;; Commentary:
;;;
;;; Test properties of the battery module.
;;; Some hints for adding and debugging tests:
;;;
;;;   POWER_NOW  is in Microwatts
;;;   ENERGY_FOO is in Microwatthours
;;;

(define (call-with-temporary-battery-status-file spec proc)
  "Run PROC with a temporary file created from SPEC.  Delete the file
when returning from PROC.  SPEC should be a list of pairs such as:
 '((STATUS . Charging) (POWER_NOW . 0))"
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template (string-append directory "/goobar-tmp.XXXXXX"))
         (tmp-port (mkstemp! template))
         (file-name (port-filename tmp-port)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (for-each (lambda (arg)
                    (pk arg)
                    (format tmp-port "POWER_SUPPLY_~a=~a~%"
                            (car arg) (cdr arg)))
                  spec)
        (force-output tmp-port)
        (proc file-name))
      (lambda ()
        (delete-file file-name)))))

(test-begin "battery")

(test-equal "battery not found"
  "no-such-battery <not found>"
  (status->string (battery-status "no-such-battery")))

(test-eq "unknown status"
  'unknown!
  (call-with-temporary-battery-status-file
   '((STATUS . "unknown!")
     (ENERGY_FULL . 100)
     (ENERGY_NOW  . 100)
     (POWER_NOW   . 10))
   (lambda (file)
     (assoc-ref (status-data (battery-status file))
                'status))))

(test-equal "battery time remaining, charging, no power"
  0
  (call-with-temporary-battery-status-file
   '((STATUS . Charging)
     (ENERGY_FULL . 100)
     (ENERGY_NOW  . 10)
     (POWER_NOW   . 0))
   (lambda (file)
     (assoc-ref (status-data (battery-status file))
                'seconds-remaining))))

(test-equal "battery time remaining, discharging, no power"
  0
  (call-with-temporary-battery-status-file
   '((STATUS . Discharging)
     (ENERGY_FULL . 100)
     (ENERGY_NOW  . 10)
     (POWER_NOW   . 0))
   (lambda (file)
     (assoc-ref (status-data (battery-status file))
                'seconds-remaining))))

(test-equal "battery-power critical level, percentage"
  'critical
  (call-with-temporary-battery-status-file
   '((STATUS . Discharging)
     (ENERGY_FULL . 100000)
     (ENERGY_NOW  . 1000)
     (POWER_NOW   . 1))
   (lambda (file)
     (status-state (battery-status file)))))

(test-equal "battery-power critical level, time remaining"
  'critical
  (call-with-temporary-battery-status-file
   '((STATUS . Discharging)
     (ENERGY_FULL . 100000)
     (ENERGY_NOW  . 100000)
     (POWER_NOW   . 10000000))
   (lambda (file)
     (status-state (battery-status file)))))

(exit (zero? (test-runner-fail-count (test-end))))
