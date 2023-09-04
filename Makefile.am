guilemoduledir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
guileobjectdir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

# Treat (goobar configuration) specially because it depends on everything else.
GOBJECTS = $(SOURCES:%.scm=%.go) $(nobase_nodist_guilemodule_DATA:%.scm=%.go) goobar/configuration.go

nobase_guilemodule_DATA = $(SOURCES) $(NOCOMP_SOURCES) goobar/configuration.scm
nobase_guileobject_DATA = $(GOBJECTS)

nobase_nodist_guilemodule_DATA = \
	status/collector/disk.scm

if HAVE_PACTL
nobase_nodist_guilemodule_DATA += status/collector/pulseaudio.scm
endif

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_guileobjectDATA
$(guile_install_go_files): \
	install-nobase_guilemoduleDATA \
	install-nobase_nodist_guilemoduleDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile \
	$(GUILE_WARNINGS) -o "$@" "$<"

if HAVE_GUILE_STATVFS
STATVFS_OR_DF = statvfs
else
STATVFS_OR_DF = df
endif

do_subst = $(SED) \
	-e 's,[@]PACTL[@],$(PACTL),g' \
	-e 's,[@]DF[@],$(DF),g' \
	-e 's,[@]STATVFS_OR_DF[@],$(STATVFS_OR_DF),g'

%.scm: %.scm.in Makefile
	$(do_subst) < $< > $@

bin_SCRIPTS = \
	scripts/goobar

SOURCES = \
	status.scm \
	status/collector/backlight.scm \
	status/collector/battery.scm \
	status/collector/cpu-temperature.scm \
	status/collector/cpu-usage.scm \
	status/collector/ipv6.scm \
	status/collector/load.scm \
	status/collector/memory.scm \
	status/collector/network-rate.scm \
	status/collector/time.scm \
	goobar/annotation.scm \
	goobar/colors.scm \
	goobar/options.scm \
	goobar/output/element.scm \
	goobar/output.scm \
	goobar/processes.scm \
	goobar.scm \
	tests/helper.scm

TESTS = \
	tests/goobar/annotation.scm	\
	tests/goobar/colors.scm		\
	tests/goobar/processes.scm	\
	tests/goobar/test-output.sh	\
	tests/status/battery.scm	\
	tests/status/network-rate.scm

if HAVE_GUILE_NETLINK
SOURCES += \
	status/network.scm \
	status/nl80211.scm \
	status/collector/ethernet.scm \
	status/collector/wifi.scm
TESTS += \
	tests/status/wifi.scm

if HAVE_PACTL
TESTS += tests/goobar/test-config.sh
endif # HAVE_PACTL
endif # HAVE_GUILE_NETLINK

if HAVE_GUILE_GNUTLS
SOURCES += \
	status/http-client.scm \
	tests/http.scm
TESTS += tests/status/http-client.scm
if HAVE_GUILE_SRFI_180
SOURCES += status/collector/weather.scm
TESTS += tests/status/weather.scm
endif # HAVE_GUILE_SRFI_180
endif # HAVE_GUILE_GNUTLS

TEST_EXTENSIONS = .scm .sh

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

dist_doc_DATA = COPYING README.md

EXTRA_DIST = \
	$(SOURCES) \
	goobar/configuration.scm \
	status/collector/disk.scm.in \
	status/collector/pulseaudio.scm.in \
	$(TESTS) \
	build-aux/pre-inst-env.in \
	build-aux/test-env.in \
	guix.scm

CLEANFILES = \
	$(nobase_nodist_guilemodule_DATA) \
	$(GOBJECTS)

clean-local:
	rm -f *.log
