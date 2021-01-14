import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

CoverBackground {
    id: cover

    Label {
        x: Theme.paddingLarge
        y: Theme.paddingLarge
        color: Theme.highlightColor
        font.pixelSize: Theme.fontSizeExtraSmall
        text: "Pusfofefe"
    }

    Column {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        Label {
            anchors.horizontalCenter: parent.horizontalCenter
            color: Theme.highlightColor
            text: coverMessages
        }

        Label {
            anchors.horizontalCenter: parent.horizontalCenter
            color: Theme.primaryColor
            text: coverNewMessages
        }
    }

    CoverActionList {
        CoverAction {
            iconSource: "image://theme/icon-cover-sync"
            onTriggered: function() {
                Lisp.call("cloverlover::pf-download-messages")
                setMessagesModelTimer.running = true
            }
        }
    }
}
