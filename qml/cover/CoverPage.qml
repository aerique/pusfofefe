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
        text: ""

        Timer {
            // `*pushover-refresh*` is in seconds
            interval: 1000 * Lisp.call("cloverlover::get-pushover-refresh")
            running: cover.status == Cover.Active
            repeat: true
            onTriggered: function() {
                Lisp.call("cloverlover::pf-download-messages", true)
                coverMessage.text = Lisp.call("cloverlover::pf-cover-message")
            }
        }
    }

    CoverActionList {
        CoverAction {
            iconSource: "image://theme/icon-cover-sync"
            onTriggered: Lisp.call("cloverlover::pf-download-messages")
        }
    }
}
