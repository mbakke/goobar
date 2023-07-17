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

(define-module (status nl80211)
  #:use-module (rnrs bytevectors)
  #:use-module (netlink nl80211 scan)
  #:use-module (netlink nl80211 station)
  #:use-module (netlink constant)
  #:use-module (netlink message)
  #:use-module (netlink generic)
  #:use-module ((ip utils) #:select (get-attr))
  #:export (nl80211-bss-info nl80211-station-info))

;; Avoid pulling in the information-elements machinery.
(define (find-ssid bv)
  "Extract the SSID from BV.  Return it as string, or #f if not found."
  (let ((len (bytevector-length bv)))
    (let loop ((pos 0))
      (if (>= pos len)
          #f
          ;; Look for the first 0 byte, indicating the start of SSID.
          (if (= 0 (bytevector-u8-ref bv pos))
              (let* ((size (bytevector-u8-ref bv (+ pos 1)))
                     (out (make-bytevector size)))
                (bytevector-copy! bv (+ pos 2) out 0 size)
                (utf8->string out))
              ;; Uhhh... If we don't find the SSID right away, maybe not loop
              ;; at all?  Looking for "0" in random places is no good.  Anyway.
              (loop (+ pos 2)))))))

(define (status->symbol status)
  (cond
   ((= status NL80211_BSS_STATUS_ASSOCIATED) 'associated)
   ((= status NL80211_BSS_STATUS_AUTHENTICATED) 'authenticated)
   ((= status NL80211_BSS_STATUS_IBSS_JOINED) 'ibss-joined)
   (else 'not-supported)))

(define (nl80211-bss-info interface)
  (let* ((scan (get-scan interface))
         (msg (car scan)))
    (if (and (memv (message-kind msg) (list NL80211_CMD_GET_SCAN))
             ;; (message-data msg) may be #f in some circumstances.
             (message-data msg))
        (let ((attrs (generic-message-attrs (message-data msg))))
          (if attrs
              (let* ((bss (get-attr attrs NL80211_ATTR_BSS))
                     (bssid (get-attr bss NL80211_BSS_BSSID))
                     (status (get-attr bss NL80211_BSS_STATUS))
                     (ies (get-attr bss NL80211_BSS_INFORMATION_ELEMENTS)))
                `((bssid . ,bssid)
                  (status . ,(and status (status->symbol status)))
                  (ssid . ,(and ies (find-ssid ies)))))
              '()))
        '())))

(define (nl80211-station-info interface bssid)
  (let* ((station (get-station interface bssid))
         (msg (car station)))
    (if (and (memv (message-kind msg) (list NL80211_CMD_GET_SCAN))
             (message-data msg))
        (let* ((attrs (generic-message-attrs (message-data msg)))
               (sta-info (get-attr attrs NL80211_ATTR_STA_INFO)))
          (if sta-info
              (let* ((signal (get-attr sta-info NL80211_STA_INFO_SIGNAL))
                     (bitrate (get-attr sta-info NL80211_STA_INFO_RX_BITRATE))
                     (rate (and bitrate
                                (or (get-attr bitrate NL80211_RATE_INFO_BITRATE32)
                                    (get-attr bitrate NL80211_RATE_INFO_BITRATE)))))
                `((signal . ,signal)
                  (bitrate . ,rate)))
              '()))
        '())))
