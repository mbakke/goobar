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
  #:use-module (tests helper)
  #:use-module (tests http)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
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
        (test-assert "301 redirect"
          (let ((result (http-fetch (%local-url))))
            (and (string=? result "yay")
                 (string=? (get-output-string error-output)
                           (format #f "following redirect to ~a...~%"
                                   initial-url)))))))))

(test-eq "500 server error"
  #f
  (with-http-server '((500 "derp"))
    (http-fetch (%local-url))))


;;;
;;; Cache
;;;

(define %cache-directory (@@ (status http-client) %cache-directory))
(define cache-file-for-uri (@@ (status http-client) cache-file-for-uri))

(test-equal "cache-file-for-uri"
  ;; Ensure GnuTLS hash-direct does not change under our feet.
  "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"
  (basename (cache-file-for-uri "test")))

(test-assert "fetch with cache, new"
  (with-http-server '((200 "yay"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (http-fetch/cached (%local-url)))))))

(test-equal "fetch with cache, valid cache"
  "hi!"
  (with-http-server '((200 "yay"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (call-with-output-file (cache-file-for-uri (%local-url))
           (lambda (port) (format port "hi!")))
         (http-fetch/cached (%local-url)))))))

(test-equal "fetch with cache, expired cache"
  "yay"
  (with-http-server '((200 "yay"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (let ((cache-file (cache-file-for-uri (%local-url))))
           (call-with-output-file cache-file
             (lambda (port) (format port "nope")))
           (utime cache-file 0 0)
           (http-fetch/cached (%local-url))
           (let ((stat (stat cache-file)))
             (and (zero? (- (current-time) (stat:mtime stat)))
                  (call-with-input-file cache-file get-string-all)))))))))

(test-eq "fetch with cache, error"
  #f
  (with-http-server '((500 "derp"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (http-fetch/cached (%local-url)))))))

(test-eqv "fetch with cache, error, file size"
  0
  (with-http-server '((500 "derp"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (http-fetch/cached (%local-url))
         (stat:size (stat (cache-file-for-uri (%local-url)))))))))

(test-assert "fetch with cache, error, cached"
  (with-http-server '((500 "derp"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (let ((cache-file (cache-file-for-uri (%local-url)))
               (30s-ago (- (current-time) 30)))
           (call-with-output-file cache-file
             (lambda (port) (format port "")))
           (utime cache-file 30s-ago 30s-ago)
           (http-fetch/cached (%local-url))
           (>= (- (current-time) (stat:mtime (stat cache-file)))
               30)))))))

(test-assert "fetch with cache, error, expired cache"
  (with-http-server '((500 "derp"))
    (call-with-temporary-directory
     (lambda (dir)
       (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
         (let ((cache-file (cache-file-for-uri (%local-url))))
           (call-with-output-file cache-file
             (lambda (port) (format port "")))
           (utime cache-file 0 0)
           (http-fetch/cached (%local-url))
           (<= (- (current-time) (stat:mtime (stat cache-file)))
               1)))))))

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
        (test-assert "fetch with cache, redirect"
          (call-with-temporary-directory
           (lambda (dir)
             (with-environment-variables `(("XDG_CACHE_HOME" . ,dir))
               (let ((result (http-fetch/cached (%local-url))))
                 (and (string=? result "yay")
                      (string=? (get-output-string error-output)
                                (format #f "following redirect to ~a...~%"
                                        initial-url))))))))))))

(exit (zero? (test-runner-fail-count (test-end))))
