import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

CoverBackground {
    Label {
        x: Theme.paddingLarge
        y: Theme.paddingLarge
        color: Theme.highlightColor
        text: "Pusfofefe"
    }

    //Label {
    //    anchors.verticalCenter: parent.verticalCenter
    //    anchors.horizontalCenter: parent.horizontalCenter
    //    color: Theme.highlightColor
    //    // FIXME this does not update when refresh / sync below is used
    //    //       This has something to do with not updating the object?
    //    text: Lisp.call("cloverlover::pf-cover-message")
    //}

    CoverActionList {
        CoverAction {
            iconSource: "image://theme/icon-cover-sync"
            onTriggered: Lisp.call("cloverlover::pf-download-messages")
        }
    }
}
