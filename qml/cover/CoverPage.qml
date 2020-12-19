import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

CoverBackground {
    // XXX this should become the app icon
    Label {
        anchors.horizontalCenter: parent.horizontalCenter
        textFormat: Text.RichText
        text: "<br><b>Pusfofefe</b>"
    }

    Label {
        anchors.verticalCenter: parent.verticalCenter
        anchors.horizontalCenter: parent.horizontalCenter
        // FIXME this does not update when refresh below is used
        text: Lisp.call("cloverlover::pf-cover-message")
    }

    CoverActionList {
        CoverAction {
            iconSource: "image://theme/icon-cover-sync"
            onTriggered: Lisp.call("cloverlover::pf-download-messages")
        }
    }
}
