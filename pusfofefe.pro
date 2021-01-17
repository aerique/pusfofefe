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
    lisp/qml.lisp \
    lisp/app.lisp \
    lisp/app.asd

lisp.output = libapp.a
lisp.commands = eql5 -platform minimal $$PWD/make.lisp
lisp.input = LISP_FILES

QMAKE_EXTRA_COMPILERS += lisp

# The name of your application
TARGET = pusfofefe
PRE_TARGETDEPS += libapp.a

CONFIG += sailfishapp
LIBS += -lecl -leql5 -L. -lapp

SOURCES += src/pusfofefe.cc

DISTFILES += qml/pusfofefe.qml \
    qml/cover/CoverPage.qml \
    qml/pages/MessagesPage.qml \
    qml/pages/MessagePage.qml \
    qml/pages/SettingsPage.qml \
    qml/pages/LoginDialog.qml \
    qml/pages/AboutPage.qml \
    rpm/pusfofefe.changes.in \
    rpm/pusfofefe.changes.run.in \
    rpm/pusfofefe.spec \
    rpm/pusfofefe.yaml \
#    translations/*.ts \
    pusfofefe.desktop

SAILFISHAPP_ICONS = 86x86 108x108 128x128 172x172

# to disable building translations every time, comment out the
# following CONFIG line
# CONFIG += sailfishapp_i18n

# German translation is enabled as an example. If you aren't
# planning to localize your app, remember to comment out the
# following TRANSLATIONS line. And also do not forget to
# modify the localized app name in the the .desktop file.
# TRANSLATIONS += translations/pusfofefe-de.ts
