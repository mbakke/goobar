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
  #:use-module (gnutls)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (http-fetch http-fetch/cached))

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


;;;
;;; Caching.
;;;

(define %cache-directory
  (or (and=> (getenv "XDG_CACHE_HOME")
             (lambda (dir) (string-append dir "/goobar/http")))
      (and=> (getenv "HOME")
             (lambda (dir) (string-append dir "/.cache/goobar/http")))
      (string-append "/tmp/goobar/http-cache")))

(define (digest-sha256 str)
  ;; Return a hexadecimal SHA256 digest of STR.
  (string-join
   (map (cut format #f "~x" <>)
        (bytevector->u8-list (hash-direct digest/sha256
                                          (string->utf8 str))))
   ""))

(define (cache-file-for-uri uri)
  "Return the name of the file in the cache corresponding to URI."
  (let ((sha256 (digest-sha256 (if (uri? uri)
                                   (uri->string uri)
                                   uri))))
    (string-append %cache-directory "/" sha256)))

(define (create-cache-directory!)
  (let* ((cache %cache-directory)
         (parent (dirname cache))
         (system (dirname parent)))
    (if (file-exists? system)
        (if (file-exists? parent)
            (unless (file-exists? cache)
              (mkdir cache))
            (begin (mkdir parent) (mkdir cache)))
        (begin
          (format (current-error-port)
                  "The directory ~a does not exist, please create it.~%"
                  system)
          (exit 1)))))

(define* (fetch-with-cache uri cache #:key (headers '()))
  (let ((result (http-fetch uri #:headers headers)))
    (call-with-output-file cache
      (lambda (port)
        ;; TODO: Serve stale content if result is false and cache exist?
        (format port (or result ""))))
    result))

;; TODO: Clean up stale cache..?
(define* (http-fetch/cached uri
                            #:optional (ttl 60)
                            #:key (headers '()))
  (create-cache-directory!)

  ;; TODO: support if-modified-since..!
  (let ((cache-file (cache-file-for-uri uri)))
    (if (file-exists? cache-file)
        (let* ((now (current-time))
               (stat (stat cache-file))
               (size (stat:size stat))
               (age (- now (stat:mtime stat))))
          (cond
           ;; TODO: Asynchronously update cache.
           ((> age ttl) (fetch-with-cache uri cache-file #:headers headers))
           ;; Cache negative results for 1 minute.
           ((zero? size) (http-fetch/cached uri #:headers headers))
           (else (call-with-input-file cache-file get-string-all))))
        (fetch-with-cache uri cache-file #:headers headers))))
