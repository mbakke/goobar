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
  #:use-module (srfi srfi-19)
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
                     (keep-alive? #f)
                     (log-port (current-error-port)))
  "Return the received body from URI as a string, following redirects, granted
that the final HTTP response code was 200.  Return 'not-modified if the HTTP
response code was 304.  Return #f for any other responses."
  (with-exception-handler
      (lambda (err)
        (cond ((eq? 'getaddrinfo-error (exception-kind err))
               ;; Gracefully handle DNS lookup failure.
               (format log-port "goobar: DNS resolution failure~%")
               #f)
              ((and (eq? 'system-error (exception-kind err))
                    (string=? "connect" (exception-origin err)))
               ;; And connection failures.
               (format log-port "goobar: connect failed: ~a~%"
                       (car (exception-irritants err)))
               #f)
              ((eq? 'gnutls-error (exception-kind err))
               ;; Similar for TLS errors, e.g. when connection abruptly closes.
               (format log-port "goobar: HTTPS connection failed: `~a'~%"
                       (exception-message err))
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
          ((304)                        ;not modified
           'not-modified)
          ((301                         ;moved permanently
            302                         ;found (redirection)
            303                         ;see other
            307                         ;temporary redirection
            308)                        ;permanent redirection
           (let ((redirect (response-location response)))
             (format log-port "following redirect to ~a...~%"
                     (uri->string redirect))
             (http-fetch redirect #:method method)))
          (else
           (format log-port
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
   (map (cut format #f "~2,'0x" <>)
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

(define* (fetch-with-cache uri cache #:key
                           (headers '())
                           (log-port (current-error-port)))
  (let ((result (http-fetch uri #:headers headers #:log-port log-port)))
    (if (and (eq? result 'not-modified)
             ;; Do an additional sanity check in case a server gives 304
             ;; without us sending if-modified-since.
             (file-exists? cache))
        (let* ((now (current-time 'time-utc))
               (ts (time-second now)))
          ;; Update the timestamp to avoid probing the server until
          ;; TTL expires again.
          (utime cache ts ts)
          (call-with-input-file cache get-string-all))
        (let ((fmt (or result "")))
          (call-with-output-file cache
            (lambda (port)
              ;; TODO: Serve stale content if result is false and cache exist?
              (format port fmt)))
          result))))

;; TODO: Clean up stale cache..?
(define* (http-fetch/cached uri
                            #:optional (ttl 60)
                            #:key
                            (headers '())
                            (log-port (current-error-port)))
  (create-cache-directory!)

  ;; TODO: Support 'Expires'..!
  (let ((cache-file (cache-file-for-uri uri)))
    (if (file-exists? cache-file)
        (let* ((now (current-time 'time-utc))
               (stat (stat cache-file))
               (size (stat:size stat))
               (mtime (make-time time-utc 0 (stat:mtime stat)))
               (age (time-second (time-difference now mtime)))
               (headers `((if-modified-since . ,(time-utc->date mtime))
                          ,@headers)))
          (cond
           ;; TODO: Asynchronously update cache.
           ((> age ttl) (fetch-with-cache uri cache-file
                                          #:headers headers
                                          #:log-port log-port))
           ;; Cache negative results for 1 minute.
           ((zero? size)
            (if (> age 60)
                (fetch-with-cache uri cache-file
                                  #:headers headers
                                  #:log-port log-port)
                #f))
           (else (call-with-input-file cache-file get-string-all))))
        (fetch-with-cache uri cache-file
                          #:headers headers
                          #:log-port log-port))))
