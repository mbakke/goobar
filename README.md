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

To customize it, create `~/.config/goobar/config.scm` and have it return a list of
strings (or `<status>` objects), typically created from the various
`(status collector ...)` modules:

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
             (ice-9 format))

(define (magic-8-ball)
  (format #f "🎱 says: ~a" (if (odd? (random 99)) "yes" "no")))

(list (format-disk-status (disk-status "/"))
      (format-disk-status (disk-status "/home"))
      (let ((ipv6 (ipv6-status)))
        ;; Don't bother printing the full IPv6 address.
        (format #f "IPv6: ~a" (if (status-good? ipv6) "✔" "❌")))
      (let ((wifi-status (wifi-status "wlp0s20f3")))
        (if (assoc-ref wifi-status 'connected?)
            (format #f "~a ~a (~a, ~a)"
                    (assoc-ref wifi-status 'icon)
                    (assoc-ref wifi-status 'ssid)
                    (format-signal (assoc-ref wifi-status 'signal))
                    (format-bitrate (assoc-ref wifi-status 'bitrate)))
            (format #f "~a down" (assoc-ref wifi-status 'icon))))
      (format-battery-status (battery-status "BAT0"))
      (format-load-status (load-status))
      (format-cpu-usage-status (cpu-usage-status))
      (format-pulseaudio-status (pulseaudio-status "0"))
      (magic-8-ball)
      (format-time-status (time-status) "%d/%m %T"))
```

# Contributing

Pull requests welcome!  There are a bunch of TODOs in the code, and probably
even more bugs.  More status collectors are nice to have.

Planned features that are not yet implemented:

* Colors
* Bidirectional communication (click events)
* More output formats (dzen2, xmobar, etc)
* Tests

# License

Unless otherwise noted, all code is GNU General Public License version 3,
or (at your option) any later version.
