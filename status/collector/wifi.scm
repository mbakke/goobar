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

(define-module (status collector wifi)
  #:use-module (status)
  #:use-module (status network)
  #:use-module (status nl80211)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (wifi-status
            format-signal
            format-bitrate
            format-wifi-status))

(define (signal->percentage signal)
  ;; This algorithm is taken from i3status (who took it from NetworkManager).
  (let ((noise-floor -90)
        (signal-max -20))
    (- 100 (* 70 (/ (- signal-max signal) (- signal-max noise-floor))))))

(define* (wifi-status interface #:key (quality-treshold 50))
  (let* ((bss (nl80211-bss-info interface))
         (status (assoc-ref bss 'status))
         (connected? (if (member status '(associated ibss-joined)) #t #f))
         (bssid (assoc-ref bss 'bssid))
         (ssid (assoc-ref bss 'ssid))
         (sta (if bssid (nl80211-station-info interface bssid) '()))
         (signal (assoc-ref sta 'signal))
         (quality (and signal (signal->percentage signal)))
         (bitrate (assoc-ref sta 'bitrate))
         (ips (assoc-ref (get-ip-addresses) interface))
         (ipv4 (or (assoc-ref ips 'ipv4) '()))
         (ipv6 (or (assoc-ref ips 'ipv6) '()))
         ;; For convenience, add an "IP" key containing the first routable
         ;; IP address, which is what users want in most cases.
         (ip (find routable? ipv4)))
    (make-status
     "📶"
     (if (and connected? ssid)
         (if (and ip (> quality quality-treshold))
             'good
             'degraded)
         'bad)
     `((interface . ,interface)
       (connected? . ,connected?)
       (ssid . ,ssid)
       (signal . ,signal)                ;dBm
       (quality . ,quality)              ;percentage
       (bitrate . ,bitrate)              ;Kb/s
       (ipv4 . ,ipv4)
       (ipv6 . ,ipv6)
       (ip . ,ip))
     format-wifi-status)))

(define (format-signal signal)
  (if signal
      (if (positive? signal)
          ;; Assume percentage.
          (format #f "~d%" signal)
          (format #f "~d dBm" signal))
      "unknown signal strength"))

(define (format-bitrate bitrate)
  (if bitrate
      (format #f "~1,1f Mb/s" (/ bitrate 10))
      "unknown bitrate"))

(define (format-wifi-status status)
  (let* ((data (status-data status))
         (icon (status-title status))
         (connected? (assoc-ref data 'connected?))
         (bitrate (assoc-ref data 'bitrate))
         (quality (assoc-ref data 'quality))
         (ssid (assoc-ref data 'ssid)))
    (if (status-bad? status)
        (format #f "~a down" icon)
        (format #f "~a (~a at ~a)~a)"
                icon
                (format-signal quality)
                ssid
                (format #f "~@[ ~a~]" (assoc-ref data 'ip))))))
