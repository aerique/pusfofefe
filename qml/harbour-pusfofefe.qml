import QtQuick 2.0
import Sailfish.Silica 1.0
import Nemo.KeepAlive 1.2
import Nemo.Notifications 1.0
import EQL5 1.0

import "pages/" as Pages

ApplicationWindow
{
    property var coverMessages: Lisp.call("cloverlover::pf-cover-messages")
    property var coverNewMessages: Lisp.call(
                                          "cloverlover::pf-cover-new-messages")

    initialPage: Component { Pages.MessagesPage { } }
    cover: Qt.resolvedUrl("cover/CoverPage.qml")
    allowedOrientations: defaultAllowedOrientations

    RemorsePopup { id: remorse }

    // My BusyLabel
    Rectangle {
        property var portrait: parent.width <= parent.height

        id: busy_rect
        objectName: "busy_rect"

        width: parent.width / 1.5
        height: portrait ? parent.width / 1.5 : parent.width / 3
        x: parent.width / 6
        y: portrait ? (parent.height / 2) - (parent.width / 3) :
                      (parent.height / 2) - (parent.width / 6)
        radius: portrait ? parent.width / 16 : parent.height / 16

        visible: false

        color: Theme.overlayBackgroundColor
        opacity: Theme.opacityOverlay

        BusyIndicator {
            id: busy_indicator
            objectName: "busy_indicator"
            size: BusyIndicatorSize.Large
            anchors.centerIn: parent
            running: false
        }

        Label {
            id: busy_text
            objectName: "busy_label"
            anchors {
                top: busy_indicator.bottom
                topMargin: Theme.paddingLarge
                horizontalCenter: parent.horizontalCenter
            }
            text: "BusyLabel text stub"
        }
    }

    BackgroundItem {
        id: feedback
        objectName: "feedback"
        anchors.fill: parent
        visible: false

        Rectangle {
            property var portrait: parent.width <= parent.height

            width: parent.width / 1.5
            height: portrait ? parent.width / 1.5 : parent.width / 3
            x: parent.width / 6
            y: portrait ? (parent.height / 2) - (parent.width / 3) :
                          (parent.height / 2) - (parent.width / 6)
            radius: portrait ? parent.width / 16 : parent.height / 16

            color: Theme.overlayBackgroundColor
            opacity: Theme.opacityOverlay

            Icon {
                id: warningIcon
                y: Theme.paddingLarge
                anchors.horizontalCenter: parent.horizontalCenter
                source: "image://theme/icon-l-weather-d200-light"
                //source: "image://theme/icon-l-attention"
            }

            Label {
                id: feedbackLabel
                objectName: "feedbackLabel"

                width: parent.width - 2 * Theme.paddingLarge
                // FIXME `Unable to assign QString to QQuickAnchorLine`, WTF
                //anchors.top: warningIcon.bottom + Theme.paddingLarge
                anchors.top: warningIcon.bottom
                x: Theme.paddingLarge

                color: Theme.secondaryHighlightColor
                wrapMode: Text.WordWrap
                text: ""
            }
        }

        onClicked: visible = false
    }

    Notification {
        objectName: "notification"  // so it can be called from Lisp
        appName: "Pusfofefe"
        summary: "Notification summary stub"
        previewSummary: "Notification previewSummary stub"
        body: "Notification body stub"
        previewBody: "Notification previewBody stub"
    }

    BackgroundJob {
        id: pushoverRefreshTimer
        frequency: eval(Lisp.call(
            "cloverlover::get-pushover-refresh-for-bgjob"))
        enabled: true
        onTriggered: function() {
            Lisp.call("cloverlover::pf-download-messages", true)
            setMessagesModelTimer.running = true
            pushoverRefreshTimer.finished()
        }
    }

    // XXX This hack exists because in the `cloverlover.lisp` function
    //     `download-messages-thread` we cannot update the messagesModel.
    //     Better solutions are appreciated.
    Timer {
        id: setMessagesModelTimer
        interval: 500
        running: false
        repeat: true
        onTriggered: function() {
            if (Lisp.call("cloverlover::update-model-p")) {
                Lisp.call("cloverlover::set-messages-model")
                coverMessages = Lisp.call("cloverlover::pf-cover-messages")
                coverNewMessages =
                                Lisp.call("cloverlover::pf-cover-new-messages")
                setMessagesModelTimer.running = false
            }
        }
    }
}
