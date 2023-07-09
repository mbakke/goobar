(use-modules (guix packages)
             (guix git)
             (guix git-download)
             (guix build-system gnu)
             (guix gexp)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages flex)
             (gnu packages gettext)
             (gnu packages gperf)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages pulseaudio)
             (gnu packages texinfo)
             (gnu packages web)
             ((guix licenses) #:select (gpl3+)))

;; Use my custom fork of Guile-Netlink with NL80211 support until
;; the changes are upstreamed.
(define guile-netlink/nl80211
  (package
    (inherit guile-netlink)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mbakke/guile-netlink")
                    (commit "77b979b585d07da068670aadf26345f31f525546")))
              (sha256
               (base32
                "1wsqiqm3sqyar3vxmalcirhi811wjgia7mjh8fxgqsashdybyfr8"))))))

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
        (add-after 'install 'wrap-executable
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out #$output)
                   (bin (string-append out "/bin"))
                   (site (string-append out "/share/guile/site"))
                   (netlink #$(this-package-input "guile-netlink"))
                   (deps (list netlink)))
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
  (inputs (list coreutils-minimal guile/statvfs guile-netlink/nl80211 pulseaudio))
  (home-page "https://github.com/mbakke/goobar")
  (synopsis "Status line generator")
  (description
   "Goobar is a status line generator for desktop environments such as Sway or i3.
It is similar to @command{i3status}, but written in GNU Guile.")
  (license gpl3+))
