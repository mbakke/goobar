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

(define-module (goobar processes)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:export (save-pid-file))

;;; Commentary:
;;;
;;; Save the current process ID to a file.  Useful when other programs
;;; need to signal goobar.  If multiple goobar processes are running,
;;; the PID file refers to the first instance.
;;;
;;; Code:

(define (save-pid-file)
  (let* ((pid-directory (pid-directory))
         (file-name (string-append pid-directory "/pid"))
         (pid (getpid))
         (previous (read-pid file-name)))
    (unless (file-exists? pid-directory)
      (create-pid-directory!))
    (if previous
        (if (process-alive? previous)
            (format (current-error-port)
                    "goobar: PID file claimed by ~a, not saving own PID (~d)~%"
                    previous pid)
            (write-pid file-name pid))
        (write-pid file-name pid))))

(define (pid-directory)
  (string-append (or (getenv "XDG_RUNTIME_DIR") (getenv "TMPDIR") "/tmp")
                 "/goobar"))

(define (create-pid-directory!)
  (with-exception-handler
      (lambda (err)
        (let ((kind (exception-kind err))
              (args (exception-args err)))
          ;; Fail gracefully upon exception.
          (format (current-error-port)
                  "goobar: failed to create PID directory: ~a~%"
                  (car (exception-irritants err)))
          (exit 1)))
    (lambda ()
      (mkdir (pid-directory)))
    #:unwind? #t))

(define (read-pid file)
  ;; Return the number contained in FILE as an integer.  If FILE does not exist,
  ;; or do not contain an integer, return #f.
  (if (file-exists? file)
      (let ((contents (call-with-input-file file
                        (lambda (port)
                          (string-trim-right
                           (get-string-n port 20))))))
        (if (string-every char-set:digit contents)
            (string->number contents)
            #f))
      #f))

(define (write-pid file-name pid)
  (call-with-output-file file-name
    (lambda (port)
      (format port "~d~%" pid))))

(define (process-alive? pid)
  (file-exists? (string-append "/proc/" (number->string pid))))
