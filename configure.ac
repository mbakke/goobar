AC_INIT(goobar, 0.1)

AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_SRCDIR(configure)

AM_INIT_AUTOMAKE([foreign -Wall -Wno-portability])

AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env:build-aux/test-env.in], [chmod +x test-env])
AC_CONFIG_FILES([scripts/goobar], [chmod +x scripts/goobar])
AC_CONFIG_FILES([ Makefile ])

dnl Check common programs
AC_PROG_SED

dnl Check for the required programs.
GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

dnl Compute complete installation directories for .scm and .go files
dnl so that the Goobar executable can automagically add them to Guile's
dnl load path.
goobar_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
goobar_libdir="`eval echo $libdir | sed -e"s|NONE|$goobar_prefix|g"`"
guilemoduledir="`eval echo ${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION | sed -e"s|NONE|$goobar_prefix|g"`"
guileobjectdir="`eval echo ${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache | sed -e"s|NONE|$goobar_prefix|g"`"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

dnl Check for required Guile modules.
GUILE_MODULE_AVAILABLE([have_guile_gnutls], [(gnutls)])
AS_IF([test ! "x$have_guile_gnutls" = "xyes"], [
  AC_MSG_WARN([Guile-GnuTLS not found, HTTP functionality unavailable])
])
AM_CONDITIONAL([HAVE_GUILE_GNUTLS], [test "x$have_guile_gnutls" = "xyes"])

GUILE_MODULE_AVAILABLE([have_guile_srfi_180], [(srfi srfi-180)])
AS_IF([test ! "x$have_guile_srfi_180" = "xyes"], [
  AC_MSG_WARN([SRFI-180 not found, JSON-based collectors unavailable.])
])
AM_CONDITIONAL([HAVE_GUILE_SRFI_180], [test "x$have_guile_srfi_180" = "xyes"])

GUILE_MODULE_AVAILABLE([have_guile_netlink], [(netlink connection)])
AS_IF([test ! "x$have_guile_netlink" = "xyes"], [
  AC_MSG_WARN([Guile-Netlink not found, ethernet and wifi information unavailable.])
])
AM_CONDITIONAL([HAVE_GUILE_NETLINK], [test "x$have_guile_netlink" = "xyes"])

AS_IF([test "x$have_guile_netlink" = "xyes"], [
  GUILE_MODULE_AVAILABLE([have_guile_netlink_nl80211], [(netlink nl80211)])
  AS_IF([test ! "x$have_guile_netlink_nl80211" = "xyes"], [
    AC_MSG_ERROR([Guile-Netlink is too old (1.3.0 or higher is required)])
  ])
])
AM_CONDITIONAL([HAVE_GUILE_NETLINK_NL80211], [test "x$have_guile_netlink_nl80211" = "xyes"])

GUILE_MODULE_CHECK([have_guile_statvfs], [(guile)], [(statvfs (getcwd))], ["statvfs"])
AS_IF([test ! "x$have_guile_statvfs" = "xyes"], [
  AC_MSG_NOTICE([statvfs is not available in Guile, deferring to `df'])
  AC_ARG_VAR(DF, [df path])
  AC_PATH_PROG(DF, [df])
  if test -z "$DF"
  then
    AC_MSG_ERROR([df not found either, please install coreutils or upgrade Guile!])
  fi
])
AM_CONDITIONAL([HAVE_GUILE_STATVFS], [test "x$have_guile_statvfs" = "xyes"])


AC_ARG_WITH([pulseaudio],
  AS_HELP_STRING([--without-pulseaudio], [Disable pulseaudio support]))

AS_IF([test "x$with_pulseaudio" != "xno"],[
  AC_ARG_VAR(PACTL, [pactl path])
  AC_PATH_PROG(PACTL, [pactl])
  if test -z "$PACTL"
  then
    AC_MSG_ERROR([Pulseaudio requested, but pactl not found!])
  fi
])
AM_CONDITIONAL([HAVE_PACTL], [test ! "x$PACTL" = "x"])

AC_OUTPUT
