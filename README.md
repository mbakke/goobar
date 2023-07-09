goobar
======

**goobar** is a status line generator similar in spirit to
[`i3status(1)`](https://i3wm.org/docs/i3status.html).  The main difference is
that goobar is written in [GNU Guile](https://www.gnu.org/software/guile/) and
highly extensible.

It consists of two distinct parts: a set of *status collectors*, and a program
that converts their results to a format that can be consumed by window managers
such as Sway or i3.

# Installation

First make sure Guile and [Guile-Netlink](https://git.lepiller.eu/guile-netlink/)
are installed and usable on your system.  Then, from a source checkout, run:

```
$ autoreconf -vif
$ ./configure && make -j $(nproc)
# At this stage, you can run the program as:
$ ./pre-inst-env goobar
# Or install it:
$ sudo make install
```

If you have [GNU Guix](https://guix.gnu.org/) on your system, you can install
goobar with e.g. `guix package -p /tmp/goobar -f guix.scm` and then run it as
`/tmp/goobar/bin/goobar`.

# Configuration

By default, the output is similar to that of *i3status*:

```
$ goobar
IPv6: ❌ | 🖴 81% | 📶 Home Wifi (68%, 144.4 Mb/s) | E: down | 🔋 75.62% 06:35 | 🔊 99% | 🏋️ 0.40 | 🌡️46°C | 6.7 GiB | 2023-06-26 20:49:05
IPv6: ❌ | 🖴 81% | 📶 Home Wifi (69%, 144.4 Mb/s) | E: down | 🔋 75.55% 06:31 | 🔊 99% | 🏋️ 0.44 | 🌡️47°C | 6.7 GiB | 2023-06-26 20:49:10
[...]
```

It can be customized by creating `~/.config/goobar/config.scm`.  This file
should return a list of either `<status>` objects, or plain strings.

(note: strings can be "painted" by calling `(colorize "foo" "#FFC0CB")`)

Example:

```
(use-modules (status)
             (status collector battery)
             (status collector cpu-usage)
             (status collector disk)
             (status collector ipv6)
             (status collector load)
             (status collector pulseaudio)
             (status collector time)
             (status collector wifi)
             (goobar colors)
             (ice-9 format))

(define (magic-8-ball)
  (format #f "🎱 says: ~a" (if (odd? (random 99)) "yes" "no")))

(list (disk-status "/" #:degraded-threshold "80%")
      (disk-status "/home")
      (ipv6-status
       ;; Don't bother printing the full IPv6 address.
       #:format (lambda (status)
                  (if (status-good? status) "IPv6: ✔" "IPv6: ❌")))
      (wifi-status
       "wlp0s20f3"
       #:format (lambda (status)
                  (let ((icon (status-title status))
                        (data (status-data status)))
                    (if (status-bad? status)
                        (format #f "~a down" icon)
                        (format #f "~a ~a (~a%, ~a)"
                                icon
                                (assoc-ref data 'ssid)
                                (assoc-ref data 'quality)
                                (format-bitrate (assoc-ref data 'bitrate)))))))
      (battery-status "BAT0")
      (load-status '5min)
      (cpu-usage-status)
      (pulseaudio-status "0")
      (magic-8-ball)
      (time-status "%d/%m %T"))
```

# Contributing

Pull requests welcome!  There are a bunch of TODOs in the code, and probably
even more bugs.  More status collectors are nice to have.

Planned features that are not yet implemented:

* Bidirectional communication (click events)
* More output formats (dzen2, xmobar, etc)
* Tests

# License

Unless otherwise noted, all code is GNU General Public License version 3,
or (at your option) any later version.
