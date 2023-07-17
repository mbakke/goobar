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

(define-module (test-network-rate)
  #:use-module (status)
  #:use-module (status collector network-rate)
  #:use-module (srfi srfi-64))

;;;
;;; Commentary:
;;;
;;; Test properties of the network rate collector.
;;;

(test-begin "network-rate")

(test-equal "first run has no data"
  "lo: <no data>"
  (status->string (network-rate-status "lo")))

(test-eq "running twice in short interval works"
  'neutral
  (let ((first (network-rate-status "lo"))
        (second (network-rate-status "lo")))
    (status-state second)))

(exit (zero? (test-runner-fail-count (test-end))))
