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

(define-module (status http-client)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-71)
  #:export (http-fetch))

;;; Commentary:
;;;
;;; Simple HTTP client that follows redirects.
;;;
;;; Code:

(define (decode response body)
  ;; Attempt to decode more content-types than those supported by
  ;; (web client) ... this can be removed for Guile 3.2.10.
  (let ((content-type (car (response-content-type response))))
    (cond
     ((text-content-type? content-type)
      (utf8->string body))
     ((eq? 'application/json content-type)
      (utf8->string body))
     (else body))))

(define* (http-fetch uri #:key
                     (method 'GET)
                     (headers '())
                     (keep-alive? #f))
  ;; Return the data from URI as a string, following redirects.
  ;; Return #f if the final response is anything other than 200.
  (with-exception-handler
      (lambda (err)
        (cond ((eq? 'getaddrinfo-error (exception-kind err))
               ;; Gracefully handle DNS lookup failure.
               #f)
              ((eq? 'gnutls-error (exception-kind err))
               ;; Similar for TLS errors, e.g. when connection abruptly closes.
               ;; TODO: Should probably give the user a clue somehow.
               #f)
              (else (raise-exception err))))
    (lambda ()
      (let ((response body (http-request uri
                                         #:decode-body? #f
                                         #:method method
                                         #:headers `((user-agent . "Goobar")
                                                     ,@headers)
                                         #:keep-alive? keep-alive?)))
        (case (response-code response)
          ((200)                        ;OK
           (decode response body))
          ((301                         ;moved permanently
            302                         ;found (redirection)
            303                         ;see other
            307                         ;temporary redirection
            308)                        ;permanent redirection
           (let ((redirect (response-location response)))
             (format (current-error-port) "following redirect to ~a...~%"
                     (uri->string redirect))
             (http-fetch redirect #:method method)))
          (else
           (format (current-error-port)
                   "failed to fetch ~a: ~d ~a~%"
                   uri
                   (response-code response)
                   (response-reason-phrase response))
           #f))))
    #:unwind? #t))
