(use-modules (guix packages)
             (guix git)
             (guix git-download)
             (guix build-system gnu)
             (guix gexp)
             (gnu packages autotools)
             (gnu packages flex)
             (gnu packages gettext)
             (gnu packages gperf)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages pulseaudio)
             (gnu packages tls)
             (gnu packages texinfo)
             (gnu packages web)
             ((guix licenses) #:select (gpl3+))
             (ice-9 match)
             (srfi srfi-1))

;; Use my custom fork of Guile-Netlink with NL80211 support until
;; the changes are upstreamed.
(define guile-netlink/nl80211
  (package
    (inherit guile-netlink)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mbakke/guile-netlink")
                    (commit "c259626029e943435861bef02ba44349c95b422c")))
              (sha256
               (base32
                "1zb8lmigafkcyhmy2caly6rxz42cilg49nwvka15r5q145d3gb7j"))))))

;; ... and a custom Guile with statvfs bindings ...
(define guile/statvfs
  (package
    (inherit guile-3.0)
    (version (string-append (package-version guile-3.0) "+"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mbakke/guile")
                    (commit "6fbd1096bbea2a58f888177f2d52148761b288c8")))
              (sha256
               (base32
                "18xrjlhmxq7crsx5vw5cl7dbcnmqznpxphsgnil1byalinb3j5yd"))))
    (native-inputs
     (modify-inputs (package-native-inputs guile-3.0)
       (prepend autoconf automake flex gnu-gettext gperf libtool texinfo)))))

(define guile-dependency?
  (match-lambda
    ((label pkg)
     (if (string-prefix? "guile-" label)
         pkg #f))
    ((? package? pkg)                   ;future-proof!
     (if (string-prefix? "guile-" (package-name pkg))
         pkg #f))))

(package
  (name "goobar")
  (version "0.1")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system gnu-build-system)
  (arguments
   (list
    #:modules `((ice-9 match) (ice-9 ftw) ,@%gnu-build-system-modules)
    #:phases
    #~(modify-phases %standard-phases
        (add-before 'check 'set-XDG_CACHE_HOME
          (lambda _
            (setenv "XDG_CACHE_HOME" "/tmp")))
        (add-after 'install 'wrap-executable
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out #$output)
                   (bin (string-append out "/bin"))
                   (site (string-append out "/share/guile/site"))
                   (deps '#$(filter-map guile-dependency?
                                        (package-transitive-inputs this-package))))
              (match (scandir site)
                (("." ".." version)
                 (let ((modules (string-append site "/" version))
                       (compiled-modules (string-append
                                          out "/lib/guile/" version
                                          "/site-ccache")))
                   (wrap-program (string-append bin "/goobar")
                     `("GUILE_LOAD_PATH" ":" prefix
                       (,modules
                        ,@(map (lambda (dep)
                                 (string-append dep
                                                "/share/guile/site/"
                                                version))
                               deps)))
                     `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                       (,compiled-modules
                        ,@(map (lambda (dep)
                                 (string-append dep "/lib/guile/"
                                                version
                                                "/site-ccache"))
                               deps)))))))))))))
  (native-inputs
   (list autoconf automake guile/statvfs jq pkg-config))
  (inputs (list guile/statvfs
                guile-gnutls
                guile-netlink/nl80211
                guile-srfi-180
                pulseaudio))
  (home-page "https://github.com/mbakke/goobar")
  (synopsis "Status line generator")
  (description
   "Goobar is a status line generator for desktop environments such as Sway or i3.
It is similar to @command{i3status}, but written in GNU Guile.")
  (license gpl3+))
