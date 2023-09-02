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

(define-module (test-weather)
  #:use-module (status)
  #:use-module (status http-client)
  #:use-module (status collector weather)
  #:use-module (tests http)
  #:use-module (web client)
  #:use-module (srfi srfi-64))

;; Eh, maybe add files instead of inline pretty-format JSON.
(define %osm-location-result-denpasar
  "[
  {
    \"place_id\":523628,\
    \"licence\":\
\"Data © OpenStreetMap contributors, ODbL 1.0. http://osm.org/copyright\",\
    \"osm_type\":\"node\",\
    \"osm_id\":274113797,\
    \"lat\":\"-8.6524973\",\
    \"lon\":\"115.2191175\",\
    \"class\":\"place\",\
    \"type\":\"city\",\
    \"place_rank\":16,\
    \"importance\":0.49527342335704877,\
    \"addresstype\":\"city\",\
    \"name\":\"Denpasar\",\
    \"display_name\":\
\"Denpasar, Dangin Puri, Denpasar Timur, Bali, Nusa Tenggara, 80232, Indonesia\",\
      \"boundingbox\":[\"-8.8124973\",\"-8.4924973\",\"115.0591175\",\"115.3791175\"]\
  }
]")

;; Note: Response shortened for brevity.
(define %yr-response-denpasar
  "{
  \"properties\": {
    \"meta\": {
      \"updated_at\": \"2023-08-15T09:35:22Z\",
      \"units\": {
        \"air_pressure_at_sea_level\": \"hPa\",
        \"air_temperature\": \"celsius\",
        \"cloud_area_fraction\": \"%\",
        \"precipitation_amount\": \"mm\",
        \"relative_humidity\": \"%\",
        \"wind_from_direction\": \"degrees\",
        \"wind_speed\": \"m/s\"
      }
    },
    \"timeseries\": [
      {
        \"time\": \"2023-08-15T10:00:00Z\",
        \"data\": {
          \"instant\": {
            \"details\": {
              \"air_pressure_at_sea_level\": 1012.9,
              \"air_temperature\": 25.5,
              \"cloud_area_fraction\": 96.9,
              \"relative_humidity\": 79.1,
              \"wind_from_direction\": 130.9,
              \"wind_speed\": 6.6
            }
          },
          \"next_1_hours\": {
            \"summary\": {
              \"symbol_code\": \"partlycloudy_night\"
            },
            \"details\": {
              \"precipitation_amount\": 0
            }
          }
        }
      }
    ]
  }
}")

(test-begin "weather")

(test-equal "location->coordinates"
  '("-8.65" . "115.22")
  (with-http-server `((200 ,%osm-location-result-denpasar))
    ;; TODO: How to mock with https_proxy instead of passing #:url?
    (location->coordinates "Denpasar,Bali,Indonesia"
                           #:url (%local-url))))

(test-equal "location->coordinates, precise"
  '("-8.6524973" . "115.2191175")
  (with-http-server `((200 ,%osm-location-result-denpasar))
    (location->coordinates "Denpasar,Bali,Indonesia" #t
                           #:url (%local-url))))

(test-assert "weather-status, missing coordinates"
  (let ((status (weather-status #f)))
    (and (eqv? 'bad (status-state status))
         (string=? "<no coordinates>" (status-data status))
         (string=? "N/A" (status->string status)))))

(test-equal "weather-status, valid"
  "🌗 26°C 🌬 6.6 m/s ↖"
  (with-http-server `((200 ,%yr-response-denpasar))
    (status->string
     (weather-status '("-8.6524973" . "115.2191175")
                     #:url (%local-url)))))

(test-equal "weather-status, short"
  "🌗 26°C"
  (with-http-server `((200 ,%yr-response-denpasar))
    (status->short-string
     (weather-status '("-8.6524973" . "115.2191175")
                     #:url (%local-url)))))

(test-assert "weather-status, server error"
  (with-http-server '((500 "nope"))
    (let ((status (weather-status '("0" . "0") #:url (%local-url))))
      (and (eqv? 'bad (status-state status))
           (string=? "<fetch failed>" (status-data status))
           (string=? "N/A" (status->string status))))))

;; Commented code is no good, but serves as a reminder to one day
;; handle timeout properly...
;; (test-equal "weather-status, timeout"
;;   "<fetch failed>"
;;   (status->string (weather-status '("0" . "0")
;;                                   ;; As per RFC 5737.
;;                                   #:url "http://192.0.2.1")))

(test-assert "wind-from-direction"
  (and (string=? (wind-from-direction->icon 0) "⬇")
       (string=? (wind-from-direction->icon 45) "↙")
       (string=? (wind-from-direction->icon 90) "⬅")
       (string=? (wind-from-direction->icon 135) "↖")
       (string=? (wind-from-direction->icon 180) "⬆")
       (string=? (wind-from-direction->icon 225) "↗")
       (string=? (wind-from-direction->icon 270) "➡")
       (string=? (wind-from-direction->icon 315) "↘")
       (string=? (wind-from-direction->icon 360) "?")
       (string=? (wind-from-direction->icon 3600) "?")
       (string=? (wind-from-direction->icon #f) "?")))

(exit (zero? (test-runner-fail-count (test-end))))
