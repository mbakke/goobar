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

(define-module (goobar configuration)
  #:use-module (status collector battery)
  #:use-module (status collector cpu-temperature)
  #:use-module (status collector disk)
  #:use-module (status collector ethernet)
  #:use-module (status collector ipv6)
  #:use-module (status collector load)
  #:use-module (status collector memory)
  #:use-module (status collector time)
  #:use-module (status collector wifi)
  #:use-module (netlink constant)
  #:use-module (ip link)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (default-configuration))

(define (default-configuration)
  (let* ((network-links (filter (lambda (link)
                                  (not (equal? (link-type link) ARPHRD_LOOPBACK)))
                                (get-links)))
         ;; XXX: Is there a more reliable way to distinguish wifi vs eth devices?
         (wireless-devices (filter-map (lambda (link)
                                         (let ((name (link-name link)))
                                           (if (string-prefix? "w" name)
                                               name
                                               #f)))
                                       network-links))
         (ethernet-devices (filter-map (lambda (link)
                                         (let ((name (link-name link)))
                                           (if (string-prefix? "e" name)
                                               name
                                               #f)))
                                       network-links))
         (battery? (file-exists? "/sys/class/power_supply/BAT0/uevent")))
    `(,(ipv6-status)
      ,(format-disk-status (disk-status "/"))
      ,@(map (lambda (if) (wifi-status if)) wireless-devices)
      ,@(map (lambda (if) (ethernet-status if)) ethernet-devices)
      ,@(if battery?
            (list (format-battery-status (battery-status "BAT0")))
            '())
      ,(load-status)
      ,@(cond
         ((file-exists? "/sys/class/thermal/thermal_zone0/temp")
          (list (cpu-temperature-status)))
         ((file-exists? "/sys/class/hwmon/hwmon0/temp1_input")
          (list (cpu-temperature-status "/sys/class/hwmon/hwmon0/temp1_input")))
         (else '()))
      ,(format-memory-status (memory-status))
      ,(format-time-status (time-status)))))
