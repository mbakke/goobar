;;; This file is part of Goobar.
;;;
;;; Copyright © 2023 Marius Bakke
;;;
;;; Goobar is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Foobar is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Goobar. If not, see <https://www.gnu.org/licenses/>.

(define-module (goobar)
  #:use-module (goobar output)
  #:autoload (goobar configuration) (default-configuration)
  #:export (goobar-main))

(define %config-file
  ;; TODO: Loop XDG_CONFIG_DIRS, check $HOME, command line arguments, etc.
  (let ((xdg-config-home (getenv "XDG_CONFIG_HOME")))
    (and xdg-config-home
         (file-exists? (string-append xdg-config-home "/goobar/config.scm"))
         (string-append xdg-config-home "/goobar/config.scm"))))

;; TODO: Handle signals, exceptions.
(define* (goobar-main #:optional args #:rest rest)
  ;; TODO: Override printer in config or command line.
  (let* ((printer (cond
                   ((isatty? (current-output-port))
                    (format (current-error-port)
                            "goobar: auto-detected 'term' output~%")
                    (get-status-printer 'term))
                   (else
                    (format (current-error-port)
                            "goobar: using 'i3bar' output~%")
                    (get-status-printer 'i3bar))))
         (header (status-printer-head printer))
         (looper (status-printer-body printer))
         (tailer (status-printer-tail printer))
         (footer (status-printer-foot printer)))
    (when header (display header))
    (while #true
      (if %config-file
          (looper (primitive-load %config-file))
          (looper (default-configuration)))
      (force-output)
      ;; TODO: Align sleeps with next second and minute, i3status style.
      ;; Also make interval configurable..!
      (sleep 5)
      (when tailer (display tailer)))
    (when footer (display footer))))
