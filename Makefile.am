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

bin_SCRIPTS = \
	scripts/goobar

SOURCES = \
	status/network.scm \
	status/nl80211.scm \
	status/collector/battery.scm \
	status/collector/cpu-temperature.scm \
	status/collector/cpu-usage.scm \
	status/collector/disk.scm \
	status/collector/ethernet.scm \
	status/collector/ipv6.scm \
	status/collector/load.scm \
	status/collector/memory.scm \
	status/collector/pulseaudio.scm \
	status/collector/time.scm \
	status/collector/wifi.scm \
	goobar/configuration.scm \
	goobar/output.scm \
	goobar.scm

EXTRA_DIST = \
	$(SOURCES) \
	pre-inst-env.in \
	guix.scm \
	goobar/external.scm.in

CLEANFILES = \
	$(GOBJECTS) \
	goobar/external.scm
