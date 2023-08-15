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

(define-module (test-http-client)
  #:use-module (status http-client)
  #:use-module (tests http)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-64))

(test-begin "http-client")

(test-equal "yay"
  (with-http-server '((200 "yay"))
    (http-fetch (%local-url))))

(with-http-server `((200 "yay"))
  (let* ((initial-url (%local-url))
         (redirect (build-response #:code 301
                                   #:headers
                                   `((location
                                      . ,(string->uri initial-url)))))
         (error-output (open-output-string)))
    (parameterize ((%http-server-port 0)
                   (current-error-port error-output))
      (with-http-server `((,redirect ""))
        (test-assert "redirect"
          (begin
            (let ((result (http-fetch (%local-url))))
              (and (string=? result "yay")
                   (string=? (get-output-string error-output)
                             (format #f "following redirect to ~a...~%"
                                     initial-url))))))))))

(test-eq #f
  (with-http-server '((500 "derp"))
    (http-fetch (%local-url))))

(exit (zero? (test-runner-fail-count (test-end))))
