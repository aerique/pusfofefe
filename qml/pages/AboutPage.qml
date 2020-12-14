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
                text: "<style>a:link { color: " + Theme.primaryColor +
                      "; }</style>Pusfofefe is a Pushover client written in " +
                      "Common Lisp by Erik Winkels &lt;<a href='mailto:" +
                      "aerique@xs4all.nl'>aerique@xs4all.nl</a>&gt;."
            }

            Label {
                width: parent.width
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                textFormat: Text.RichText
                onLinkActivated: Qt.openUrlExternally(link)
                text: "<style>a:link { color: " + Theme.primaryColor +
                      "; }</style>It was built on the <a href='" +
                      "https://redmine.casenave.fr/projects/eql5-sfos/" +
                      "repository/44/revisions/master/show'>eql5-sfos</a> " +
                      "template by Renaud Casenave-Péré who also packaged " +
                      "ECL and EQL5 for Sailfish OS."
            }

            Label {
                width: parent.width
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                textFormat: Text.RichText
                onLinkActivated: Qt.openUrlExternally(link)
                text: "<style>a:link { color: " + Theme.primaryColor +
                      "; }</style><a href='https://gitlab.com/" +
                      "embeddable-common-lisp/ecl'>ECL</a> is used as the " +
                      "Common Lisp implementation."
            }

            Label {
                width: parent.width
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                textFormat: Text.RichText
                onLinkActivated: Qt.openUrlExternally(link)
                text: "<style>a:link { color: " + Theme.primaryColor +
                      "; }</style><a href='https://gitlab.com/eql/EQL5'>EQL5" +
                      "</a> courtesy of P. Ruetz."
            }
        }
    }
}
