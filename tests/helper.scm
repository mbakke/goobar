;;; This file is part of Goobar.
;;;
;;; Copyright © 2023 Marius Bakke
;;; Copyright © 2013, 2014, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests helper)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (call-with-temporary-directory
            with-environment-variables))

;;; Commentary:
;;;
;;; This contains various helpers for the test suite.
;;;
;;; Code:

(define* (delete-file-recursively dir
                                  #:key follow-mounts?)
  "Delete DIR recursively, like `rm -rf', without following symlinks.  Don't
follow mount points either, unless FOLLOW-MOUNTS? is true.  Report but ignore
errors."
  (let ((dev (stat:dev (lstat dir))))
    (file-system-fold (lambda (dir stat result)  ;enter?
                        (or follow-mounts?
                            (= dev (stat:dev stat))))
                      (lambda (file stat result) ;leaf
                        (delete-file file))
                      (const #t)                 ;down
                      (lambda (dir stat result)  ;up
                        (rmdir dir))
                      (const #t)                 ;skip
                      (lambda (file stat errno result)
                        (format (current-error-port)
                                "warning: failed to delete ~a: ~a~%"
                                file (strerror errno)))
                      #t
                      dir

                      ;; Don't follow symlinks.
                      lstat)))


(define (call-with-temporary-directory proc)
  "Call PROC with a name of a temporary directory; close the directory and
delete it when leaving the dynamic extent of this call."
  (let* ((directory (or (getenv "TMPDIR") "/tmp"))
         (template  (string-append directory "/goobar-directory.XXXXXX"))
         (tmp-dir   (mkdtemp template)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (proc tmp-dir))
      (lambda ()
        (false-if-exception (delete-file-recursively tmp-dir))))))

(define (call-with-environment-variables variables thunk)
  "Call THUNK with environment variables set.  Reset environment afterwards."
  (let ((environment (environ)))
    (dynamic-wind
      (lambda ()
        (for-each (match-lambda
                    ((variable . #false)
                     (unsetenv variable))
                    ((variable . value)
                     (setenv variable value)))
                  variables))
      thunk
      (lambda ()
        (environ environment)))))

(define-syntax-rule (with-environment-variables variables exp ...)
  "Evaluate EXP with the given environment VARIABLES set."
  (call-with-environment-variables variables
                                   (lambda () exp ...)))
