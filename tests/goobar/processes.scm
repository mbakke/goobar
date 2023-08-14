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

(define-module (test-processes)
  #:use-module (goobar processes)
  #:use-module (tests helper)
  #:use-module (srfi srfi-64))

;;; Commentary:
;;;
;;; As it turns out, managing a PID file is a lot of work.  This tests
;;; the properties of (goobar processes).
;;;
;;; Code:

(define create-pid-directory! (@@ (goobar processes) create-pid-directory!))
(define process-alive? (@@ (goobar processes) process-alive?))
(define read-pid (@@ (goobar processes) read-pid))

(test-begin "processes")

(test-assert "current process is live"
  (process-alive? (getpid)))

(test-assert "invalid process is dead"
  (let* ((pid-max (read-pid "/proc/sys/kernel/pid_max"))
         (high-pid (or (and=> pid-max (lambda (pid) (+ 1 pid)))
                       10000000000)))
    (not (or (process-alive? high-pid)
             (process-alive? -1)))))

(test-assert "create PID directory, XDG_RUNTIME_DIR"
  (call-with-temporary-directory
   (lambda (dir)
     (with-environment-variables `(("XDG_RUNTIME_DIR" . ,dir))
       (create-pid-directory!)
       (file-exists? (string-append dir "/goobar"))))))

(test-assert "create PID directory, TMPDIR"
  (call-with-temporary-directory
   (lambda (dir)
     (with-environment-variables `(("XDG_RUNTIME_DIR" . #f)
                                   ("TMPDIR" . ,dir))
       (create-pid-directory!)
       (file-exists? (string-append dir "/goobar"))))))

(test-assert "create PID directory, /tmp"
  (with-environment-variables '(("TMPDIR" . #f)
                                ("XDG_RUNTIME_DIR" . #f))
    ;; If the user already created /tmp/goobar, or do not have /tmp, we can not test.
    (or (file-exists? "/tmp/goobar")
        (not (file-exists? "/tmp"))
        (and (create-pid-directory!)
             (file-exists? "/tmp/goobar")
             (rmdir "/tmp/goobar")))))

(test-assert "create PID directory, permission denied"
  (call-with-temporary-directory
   (lambda (dir)
     (with-environment-variables `(("XDG_RUNTIME_DIR" . ,dir))
       (chmod dir #o500)
       (let ((error-output (open-output-string)))
         (parameterize ((current-error-port error-output))
           (catch 'quit
             (lambda ()
               (create-pid-directory!))
             (lambda (key value)
               (and (= 1 value)
                    (string=? (get-output-string error-output)
                              (string-append "goobar: failed to create PID directory:"
                                             " Permission denied\n")))))))))))

(test-assert "create PID file, new"
  (call-with-temporary-directory
   (lambda (dir)
     (with-environment-variables `(("XDG_RUNTIME_DIR" . ,dir))
       (let ((pid (getpid)))
         (and (save-pid-file)
              (= pid (read-pid (string-append dir "/goobar/pid")))))))))

(test-assert "create PID file, exists"
  (call-with-temporary-directory
   (lambda (dir)
     (with-environment-variables `(("XDG_RUNTIME_DIR" . ,dir))
       (let ((pid (getpid))
             (file (string-append dir "/goobar/pid")))
         (create-pid-directory!)
         (call-with-output-file file
           (lambda (port)
             (display "bogus pid!\n" port)))
         (save-pid-file)
         (= pid (read-pid file)))))))

(test-assert "create PID file, already running"
  (call-with-temporary-directory
   (lambda (dir)
     (with-environment-variables `(("XDG_RUNTIME_DIR" . ,dir))
       (let ((pid (getpid))
             (file (string-append dir "/goobar/pid"))
             (error-output (open-output-string)))
         (create-pid-directory!)
         (parameterize ((current-error-port error-output))
           (call-with-output-file file
             (lambda (port)
               (display "1\n" port)))
           (save-pid-file)
           (and (= 1 (read-pid file))
                (string=?
                 (format #f "goobar: PID file claimed by 1, not saving own PID (~d)~%"
                         pid)
                 (get-output-string error-output)))))))))

(exit (zero? (test-runner-fail-count (test-end))))
