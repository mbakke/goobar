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
  #:export (location->coordinates
            wind-from-direction->icon
            symbol-code->icon
            weather-status))

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
                   (temperature-unit (assoc-ref units 'air_temperature))
                   (wind-speed-unit (assoc-ref units 'wind_speed))
                   ;; XXX: Is this guaranteed to be chronologically sorted?
                   (timeseries (vector-ref (assoc-ref properties 'timeseries) 0))
                   (data (assoc-ref timeseries 'data))
                   (instant (assoc-ref data 'instant))
                   (details (assoc-ref instant 'details))
                   (air-temperature (assoc-ref details 'air_temperature))
                   (wind-from-direction (assoc-ref details 'wind_from_direction))
                   (wind-speed (assoc-ref details 'wind_speed))
                   (next-1h (assoc-ref data 'next_1_hours))
                   (next-1h-summary (assoc-ref next-1h 'summary))
                   (symbol-code (assoc-ref next-1h-summary 'symbol_code)))
              (make-status
               'weather
               'neutral
               `((temperature . ,air-temperature)
                 (temperature-unit . ,temperature-unit)
                 (wind-speed . ,wind-speed)
                 (wind-speed-unit . ,wind-speed-unit)
                 (wind-from-direction . ,wind-from-direction)
                 (symbol-code . ,symbol-code))
               format-weather-status format-weather-status-short))
            (make-status 'weather 'bad "<fetch failed>"
                         format-weather-status-failure)))
      (make-status 'weather 'bad "<no coordinates>"
                   format-weather-status-failure)))

(define (wind-from-direction->icon angle)
  (cond
   ((or (not angle) (>= angle 360)) "?")
   ((or (>= angle 332.5) (< angle 27.5)) "⬇")    ;north
   ((and (>= angle 27.5) (< angle 72.5)) "↙")
   ((and (>= angle 72.5) (< angle 117.5)) "⬅")   ;east
   ((and (>= angle 117.5) (< angle 162.5)) "↖")
   ((and (>= angle 162.5) (< angle 207.5)) "⬆")  ;south
   ((and (>= angle 207.5) (< angle 252.5)) "↗")
   ((and (>= angle 252.5) (< angle 297.5)) "➡")  ;west
   (else "↘")))

(define (symbol-code->icon symbol)
  ;; Note: weather icons (in PNG and SVG format) are available from:
  ;;   https://github.com/metno/weathericons/tree/main/weather
  ;; The file names correspond to the "symbol code".
  ;; Here we make do with available unicode symbols.
  (match symbol
    ("clearsky_day" "☀")
    ("fair_day" "🌤")
    ("partlycloudy_day" "⛅")
    ("clearsky_night" "🌙")
    ("fair_night" "🌜")
    ("partlycloudy_night" "🌗")
    ("fog" "🌫")
    ("cloudy" "☁")
    (or '("rain" "lightrain" "heavyrain" "sleet" "lightsleet" "heavysleet")
        "🌧")
    (or '("heavyrainshowers_day" "heavysleetshowers_day" \
          "heavysleetshowersandthunder_night" "lightrainshowers_day" \
          "lightsleetshowers_day" "rainshowers_day" "sleetshowers_day")
        "🌦")
    (or '("heavyrainshowers_night" "heavysleetshowers_night" \
          "lightrainshowers_night" "lightsleetshowers_night" \
          "rainshowers_night" "sleetshowers_night")
        "☔")
    (or '("heavyrainandthunder" "heavyrainshowersandthunder_day" \
          "heavyrainshowersandthunder_night" "heavysleetandthunder" \
          "heavysleetshowersandthunder_day" "heavysnowandthunder" \
          "heavysnowshowersandthunder_day" "heavysnowshowersandthunder_night" \
          "lightrainandthunder" "lightrainshowersandthunder_day" \
          "lightrainshowersandthunder_night" "lightsleetandthunder" \
          "lightsnowandthunder" "rainandthunder" \
          ;; Note: lightsleetthunder and lightsnowthunder have a typo (extra s);
          ;; match both cases for future-proofing (see api.met.no documentation).
          "lightssleetshowersandthunder_day" "lightssleetshowersandthunder_night" \
          "lightssnowshowersandthunder_day" "lightssnowshowersandthunder_night" \
          "lightsleetshowersandthunder_day" "lightsleetshowersandthunder_night" \
          "lightsnowshowersandthunder_day" "lightsnowshowersandthunder_night" \
          "rainshowersandthunder_day" "rainshowersandthunder_night" \
          "sleetandthunder" "sleetshowersandthunder_day" \
          "sleetshowersandthunder_night" "snowandthunder" \
          "snowshowersandthunder_day" "snowshowersandthunder_night")
        "⛈")
    (or '("snow" "lightsnow" "heavysnow") "❄")
    (or '("lightsnowshowers_day" "lightsnowshowers_night" "heavysnowshowers_day" \
          "heavysnowshowers_night" "snowshowers_day" "snowshowers_night")
        "🌨")
    (_ "?")))

(define (format-weather-status-failure status)
  "N/A")

(define* (format-weather-status status #:optional short?)
  (let* ((data (status-data status))
         (temp (assoc-ref data 'temperature))
         ;; TODO: Check and convert units!
         (normalized-temp (inexact->exact (round temp)))
         (symbol-code (assoc-ref data 'symbol-code))
         (symbol-icon (symbol-code->icon symbol-code))
         (wind-speed (assoc-ref data 'wind-speed))
         (wind-speed-unit (assoc-ref data 'wind-speed-unit))
         (wind-direction (assoc-ref data 'wind-from-direction)))
    (if short?
        (format #f "~a ~d°C" symbol-icon normalized-temp)
        (format #f "~a ~d°C 🌬 ~f ~a ~a"
                symbol-icon
                (inexact->exact (round temp))
                wind-speed
                wind-speed-unit
                (wind-from-direction->icon wind-direction)))))

(define (format-weather-status-short status)
  (format-weather-status status #t))
