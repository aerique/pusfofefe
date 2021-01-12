import QtQuick 2.0
import Sailfish.Silica 1.0

Page {
    PageHeader {
        id: pageHeader
        title: "About"
    }

    SilicaFlickable {
        width: parent.width
        anchors {
            top: pageHeader.bottom
            topMargin: Theme.paddingLarge
            left: parent.left
            leftMargin: Theme.paddingLarge
            right: parent.right
            rightMargin: Theme.paddingLarge
        }

        VerticalScrollDecorator {}

        Column {
            width: parent.width
            height: childrenRect.height
            spacing: Theme.paddingLarge

            Label {
                width: parent.width
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                textFormat: Text.RichText
                onLinkActivated: Qt.openUrlExternally(link)

                text:
                "<style>a:link { color: " + Theme.primaryColor + " }</style>" +

                "<p>Pusfofefe is a Pushover client written in Common Lisp "   +
                "by Erik Winkels &lt;<a href='mailto:aerique+pusfofefe@xs4al" +
                "l.nl'>aerique+pusfofefe@xs4all.nl</a>&gt;.</p>"              +

                "<p>The source code is available at: <a href='https://git.sr" +
                ".ht/~aerique/pusfofefe'>https://git.sr.ht/~aerique/pusfofef" +
                "e</a></p>"                                                   +

                "<p>It was built on the <a href='https://redmine.casenave.fr" +
                "/projects/eql5-sfos/repository/44/revisions/master/show'>eq" +
                "l5-sfos</a> template written by Renaud Casenave-Péré who "   +
                "also packaged ECL and EQL5 for Sailfish OS.</p>"             +

                "<p><a href='https://gitlab.com/embeddable-common-lisp/ecl'>" +
                "ECL</a> is used as the Common Lisp implementation for "      +
                "deployment while <a href='http://sbcl.org/'>SBCL</a> was "   +
                "additionally used during development.</p>"                   +

                "<p><a href='https://gitlab.com/eql/EQL5'>EQL5</a> are the "  +
                "Qt and QML bindings for ECL.</p>"                            +

                "<p>App icon made by <a href='https://www.flaticon.com/autho" +
                "rs/freepik'>Freepik</a> from <a href='https://www.flaticon." +
                "com/'>www.flaticon.com</a>.</p>"                             +

                "<p>My thanks go out to the people who have worked on "       +
                "Common Lisp, SBCL, ECL and EQL over the years!</p>"          +

                "<p>To name a few:"                                           +
                  "<ul>"                                                      +
                    "<li>Renaud Casenave-Péré"                                +
                    "<li>Marius Gerbershagen"                                 +
                    "<li>Daniel Kochmański"                                   +
                    "<li>Juan Jose Garcia Ripoll"                             +
                    "<li>P. Ruetz"                                            +
                  "</ul>"                                                     +
                "</p>"
            }
        }
    }
}
