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

(define-module (goobar)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-19)
  #:use-module (goobar options)
  #:use-module (goobar output)
  #:autoload (goobar processes) (save-pid-file)
  #:autoload (goobar configuration) (default-configuration)
  #:export (goobar-main))

(define %config-file
  ;; TODO: Loop XDG_CONFIG_DIRS, check $HOME, etc.
  (let ((xdg-config-home (getenv "XDG_CONFIG_HOME")))
    (and xdg-config-home
         (file-exists? (string-append xdg-config-home "/goobar/config.scm"))
         (string-append xdg-config-home "/goobar/config.scm"))))

(define (aligned-sleep interval)
  ;; To provide updates on (approximately) every full second, we don't
  ;; (sleep INTERVAL), but instead calculate the number of microseconds until
  ;; the closest second (+ usec-difference (- interval 1)).  Also attempt
  ;; aligning to 60 seconds such that we start on :00 every minute.
  (let ((now (current-time)))
    (usleep (+ (* 1000000 (- interval 1 (modulo (time-second now) interval)))
               (/ (- 1000000000 (time-nanosecond now)) 1000)))))

;; TODO: How to manage exceptions?
(define (goobar-main . args)
  ;; Handle USR1 so it does not terminate the program, but just interrupts the
  ;; sleep.  Useful for triggering immediate refresh on e.g. volume change.
  (sigaction SIGUSR1 (const #t))
  (let* ((options (getopt-long args %options))
         (help (option-ref options 'help #f))
         (config-file (option-ref options 'config-file %config-file))
         (interval (string->number (option-ref options 'interval "5")))
         (one-shot (option-ref options 'one-shot #f))
         (output-format (option-ref options 'output #f))
         (pid-file (option-ref options 'pid-file #f))
         (printer (cond ((string? output-format)
                         (get-output (string->symbol output-format)))
                        ((isatty? (current-output-port))
                         (get-output 'term))
                        (else (get-output 'i3bar))))
         (header (output-head printer))
         (looper (output-body printer))
         (tailer (output-tail printer))
         (footer (output-foot printer)))
    (when help (display-help-and-exit))
    (when pid-file
      (if (string? pid-file)
          (save-pid-file pid-file)
          (save-pid-file)))
    (unless output-format
      (format (current-error-port) "goobar: auto-detected '~a' output~%"
              (output-type printer)))
    (when header (display header))
    (while #true
      (if config-file
          (looper (primitive-load config-file))
          (looper (default-configuration)))
      (when one-shot (break))
      (force-output)
      (aligned-sleep interval)
      (when tailer (display tailer)))
    (when footer (display footer))))
