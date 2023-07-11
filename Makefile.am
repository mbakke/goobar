guilemoduledir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
guileobjectdir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

GOBJECTS = $(SOURCES:%.scm=%.go) goobar/external.go

nobase_guilemodule_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_guileobject_DATA = $(GOBJECTS)

nobase_nodist_guilemodule_DATA = \
	goobar/external.scm

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_guileobjectDATA
$(guile_install_go_files): install-nobase_guilemoduleDATA

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
	-e 's,[@]STATVFS_OR_DF[@],$(STATVFS_OR_DF),g'

status/collector/disk.scm: status/collector/disk.scm.in Makefile
	$(do_subst) < $< > $@

bin_SCRIPTS = \
	scripts/goobar

SOURCES = \
	status.scm \
	status/network.scm \
	status/nl80211.scm \
	status/collector/backlight.scm \
	status/collector/battery.scm \
	status/collector/cpu-temperature.scm \
	status/collector/cpu-usage.scm \
	status/collector/disk.scm \
	status/collector/ethernet.scm \
	status/collector/ipv6.scm \
	status/collector/load.scm \
	status/collector/memory.scm \
	status/collector/network-rate.scm \
	status/collector/pulseaudio.scm \
	status/collector/time.scm \
	status/collector/wifi.scm \
	goobar/annotation.scm \
	goobar/configuration.scm \
	goobar/colors.scm \
	goobar/options.scm \
	goobar/output/element.scm \
	goobar/output.scm \
	goobar.scm

TESTS = \
	tests/goobar/annotation.scm	\
	tests/goobar/colors.scm		\
	tests/goobar/test-config.sh	\
	tests/goobar/test-output.sh	\
	tests/status/battery.scm	\
	tests/status/network-rate.scm	\
	tests/status/wifi.scm

TEST_EXTENSIONS = .scm .sh

SCM_LOG_COMPILER = $(top_builddir)/test-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

SH_LOG_COMPILER = $(top_builddir)/test-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

EXTRA_DIST = \
	$(SOURCES) \
	pre-inst-env.in \
	guix.scm \
	goobar/external.scm.in

CLEANFILES = \
	$(GOBJECTS) \
	goobar/external.scm
