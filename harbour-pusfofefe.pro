# NOTICE:
#
# Application name defined in TARGET has a corresponding QML filename.
# If name defined in TARGET is changed, the following needs to be done
# to match new name:
#   - corresponding QML filename must be changed
#   - desktop icon filename must be changed
#   - desktop filename must be changed
#   - icon definition filename in desktop file must be changed
#   - translation filenames have to be changed

LISP_FILES = make.lisp \
    lisp/dependencies.lisp \
    lisp/cloverlover.lisp \
    lisp/app.lisp \
    lisp/app.asd

lisp.output = libapp.a
lisp.commands = eql5 -platform minimal $$PWD/make.lisp
lisp.input = LISP_FILES
lisp.CONFIG = combine target_predeps

QMAKE_EXTRA_COMPILERS += lisp

# The name of your application
TARGET = harbour-pusfofefe

CONFIG += sailfishapp
LIBS += -L. -lapp -lecl -leql5

CONFIG += norepl
#CONFIG += standalone

norepl|standalone:lisp.commands = eql5 -platform minimal $$PWD/make.lisp norepl

standalone {
    CONFIG += link_prl
    QT     += widgets qml multimedia network quick sql
    # Note: the line below *removes* libs!
    LIBS   -= -lecl -leql5
    LIBS   += -l:libecl.a -l:libeclatomic.a -l:libeclgc.a -l:libeclgmp.a -l:libeclffi.a -l:libeql5.a
    # XXX how to get rid of absolute path?
    LIBS   += -L/usr/lib/ecl-21.2.1 -l:libasdf.a -l:libsockets.a
}

SOURCES += src/pusfofefe.cc

DISTFILES += qml/harbour-pusfofefe.qml \
    qml/cover/CoverPage.qml \
    qml/pages/MessagesPage.qml \
    qml/pages/MessagePage.qml \
    qml/pages/SettingsPage.qml \
    qml/pages/LoginDialog.qml \
    qml/pages/AboutPage.qml \
    rpm/harbour-pusfofefe.changes.in \
    rpm/harbour-pusfofefe.changes.run.in \
    rpm/harbour-pusfofefe.spec \
    rpm/harbour-pusfofefe.yaml \
#    translations/*.ts \
    harbour-pusfofefe.desktop

SAILFISHAPP_ICONS = 86x86 108x108 128x128 172x172

# to disable building translations every time, comment out the
# following CONFIG line
# CONFIG += sailfishapp_i18n

# German translation is enabled as an example. If you aren't
# planning to localize your app, remember to comment out the
# following TRANSLATIONS line. And also do not forget to
# modify the localized app name in the the .desktop file.
# TRANSLATIONS += translations/harbour-pusfofefe-de.ts
