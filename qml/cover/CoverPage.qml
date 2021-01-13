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

    Label {
        id: coverMessage
        objectName: "coverMessage"

        anchors.horizontalCenter: parent.horizontalCenter
        anchors.verticalCenter: parent.verticalCenter

        color: Theme.highlightColor
        text: coverMsg
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
