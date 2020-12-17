import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

CoverBackground {
    Label {
        anchors.centerIn: parent
        text: "Pusfofefe"
    }

    CoverActionList {
        CoverAction {
            iconSource: "image://theme/icon-cover-sync"
            onTriggered: Lisp.call("cloverlover::pf-download-messages")
        }
    }
}
