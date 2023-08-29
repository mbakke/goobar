;;; This file is part of Goobar.
;;;
;;; Copyright © 2023 Marius Bakke
;;;
;;; Goobar is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Goobar is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Goobar. If not, see <https://www.gnu.org/licenses/>.

(define-module (status collector weather)
  #:use-module (status)
  #:use-module (status http-client)
  #:use-module (web uri)
  #:use-module (srfi srfi-180)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (location->coordinates weather-status))

(define* (location->coordinates name #:key
                                (url "https://nominatim.openstreetmap.org"))
  ;; Return a (lat . lon) pair based on NAME.  Example:
  ;; (location->coordinates "bardufoss,norway") => '("69.065637" . "18.5159237")
  (let ((data (http-fetch/cached
               (string-append url "/search?q="
                              (uri-encode name) "&format=json")
               ;; Cache coordinates for a month.
               (* 60 60 24 30))))
    (match data
      (#f #f)                           ;http-client error
      ("[]"
       (format (current-error-port) "warning: no coordinates found for \"~a\"~%" name)
       #f)
      (_
       (let* ((parsed (call-with-input-string data json-read))
              (lst (vector-ref parsed 0)))
         `(,(assoc-ref lst 'lat) . ,(assoc-ref lst 'lon)))))))

(define* (weather-status coordinates #:key
                         (url "https://api.met.no"))
  (if (and coordinates (pair? coordinates))
      (let ((weather (http-fetch/cached
                      (string-append url "/weatherapi"
                                     "/locationforecast/2.0/compact"
                                     "?lat=" (car coordinates)
                                     "&lon=" (cdr coordinates))
                      600)))
        (if weather
            (let* ((parsed (call-with-input-string weather json-read))
                   (properties (assoc-ref parsed 'properties))
                   (meta (assoc-ref properties 'meta))
                   (units (assoc-ref meta 'units))
                   (unit (assoc-ref units 'air_temperature))
                   ;; XXX: Is this guaranteed to be chronologically sorted?
                   (timeseries (vector-ref (assoc-ref properties 'timeseries) 0))
                   (data (assoc-ref timeseries 'data))
                   (instant (assoc-ref data 'instant))
                   (details (assoc-ref instant 'details))
                   (air-temperature (assoc-ref details 'air_temperature))
                   (next-1h (assoc-ref data 'next_1_hours))
                   (next-1h-summary (assoc-ref next-1h 'summary))
                   (symbol-code (assoc-ref next-1h-summary 'symbol_code)))
              ;; Note: weather icons (in PNG and SVG format) are available from
              ;;   https://github.com/metno/weathericons/tree/main/weather
              ;; The file names correspond to the "symbol code".
              (make-status
               'weather
               'neutral
               `((temperature . ,air-temperature)
                 (unit . ,unit)
                 (symbol-code . ,symbol-code))
               format-weather-status))
            (make-status 'weather 'bad "<fetch failed>"
                         format-weather-status-failure)))
      (make-status 'weather 'bad "<no coordinates>"
                   format-weather-status-failure)))

(define (format-weather-status-failure status)
  (format #f "~a" (status-data status)))

(define (format-weather-status status)
  (let* ((data (status-data status))
         (temp (assoc-ref data 'temperature)))
    ;; TODO: Check and convert units!
    (format #f "~d°C" (inexact->exact (round temp)))))
