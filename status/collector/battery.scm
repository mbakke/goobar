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

(define-module (status collector battery)
  #:use-module (status)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-71)
  #:export (battery-status format-battery-status))

(define (battery-state->symbol state)
  (match state
    ("Charging" 'charging)
    ("Discharging" 'discharging)
    ("Not charging" 'not-charging)
    ("Full" 'full)
    ;; TODO: What other states exist..?
    (_ (symbol->string state))))

(define (get-battery-status battery)
  (let ((status-file (string-append "/sys/class/power_supply/"
                                          battery "/uevent")))
    (if (file-exists? status-file)
        (let ((uevent (open-file status-file "r")))
          (let loop ((items '()))
            (let ((line (read-line uevent)))
              (if (eof-object? line)
                  (begin
                    (close-port uevent)
                    items)
                  (match (string-split line #\=)
                    ((item value)
                     (cond
                      ((string-suffix? "STATUS" item)
                       (loop (cons `(status . ,(battery-state->symbol value))
                                   items)))
                      ((string-suffix? "CYCLE_COUNT" item)
                       (loop (cons `(cycles . ,(string->number value)) items)))
                      ((string-suffix? "POWER_NOW" item)
                       (loop (cons `(power-now . ,(string->number value)) items)))
                      ((string-suffix? "ENERGY_FULL_DESIGN" item)
                       (loop (cons `(energy-full-design . ,(string->number value))
                                   items)))
                      ((string-suffix? "ENERGY_FULL" item)
                       (loop (cons `(energy-full . ,(string->number value)) items)))
                      ((string-suffix? "ENERGY_NOW" item)
                       (loop (cons `(energy-now . ,(string->number value)) items)))
                      (else (loop items)))))))))
        ;; Battery not present.
        #f)))

(define (seconds-remaining status)
  (let ((power-now (assoc-ref status 'power-now))
        (energy-now (assoc-ref status 'energy-now))
        (energy-full (assoc-ref status 'energy-full))
        (state (assoc-ref status 'status)))
    (cond
     ;; power-now can be 0 when just connected or disconnected,
     ;; in which case we can not calculate.
     ((= 0 power-now) 0)
     ((eq? 'discharging state)
      (round (* 60 60 (/ energy-now power-now))))
     ((eq? 'charging state)
      (round (* 60 60 (/ (- energy-full energy-now) power-now))))
     (else 0))))

(define* (battery-status battery #:key
                         (low-threshold 25)
                         (degraded-when-full? #t))
  (let ((status (get-battery-status battery)))
    (if status
        (let* ((seconds-remaining (seconds-remaining status))
               (energy-now (assoc-ref status 'energy-now))
               (energy-full (assoc-ref status 'energy-full))
               (energy-percent (* 100 (/ energy-now energy-full)))
               (power-now (assoc-ref status 'power-now))
               (state (assoc-ref status 'status)))
          (make-status
           (match state
             ('discharging "🔋")
             ('charging "⚡")
             ('full "🔋☻")
             ('not-charging "🔌")
             (_  "?"))
           (cond
            ((and (eq? 'discharging state)
                  (< seconds-remaining 1800)
                  ;; Prevent bad status when just disconnected but seconds-remaining
                  ;; has not been calculated yet.
                  (< energy-percent 50))
             'bad)
            ((and (eq? 'discharging state) (< energy-percent low-threshold)) 'degraded)
            ((and (eq? 'full state) degraded-when-full?) 'degraded)
            (else 'neutral))
           `((energy-percent . ,energy-percent)
             (seconds-remaining . ,seconds-remaining)
             ,@status)
           format-battery-status))
        (make-status battery 'bad #f format-battery-not-found))))

(define (format-battery-not-found status)
  (format #f "~a <not found>" (status-title status)))

(define (format-battery-status status)
  (let* ((data (status-data status))
         (seconds (assoc-ref data 'seconds-remaining))
         (hours minutes-in-seconds (truncate/ seconds 3600))
         (minutes (truncate (/ minutes-in-seconds 60)))
         (time-remaining (if (or (= 0 seconds) (= 0 minutes))
                             ""
                             (format #f " ~2,'0d:~2,'0d" hours minutes))))
    (format #f "~a ~5f%~a"
            (status-title status)
            (assoc-ref data 'energy-percent)
            time-remaining)))
