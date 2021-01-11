import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Dialog {
    Column {
        id: column
        spacing: Theme.paddingLarge
        width: parent.width

        DialogHeader {}

        TextField {
            id: pushoverEmail
            inputMethodHints: Qt.ImhNoAutoUppercase
            width: parent.width
            label: "E-mail Address"
            placeholderText: label
            text: Lisp.call("cloverlover::get-pushover-email")

            EnterKey.iconSource: "image://theme/icon-m-enter-next"
            EnterKey.onClicked: pushoverPassword.focus = true
        }

        PasswordField {
            id: pushoverPassword
            text: Lisp.call("cloverlover::get-pushover-password")

            EnterKey.iconSource: "image://theme/icon-m-accept"
            EnterKey.onClicked: accept()
        }
    }

    BackgroundItem {
        id: feedback
        objectName: "feedback"
        anchors.fill: parent
        visible: false

        Rectangle {
            property var portrait: parent.width <= parent.height

            width: parent.width / 2
            height: portrait ? parent.width / 2 : parent.width / 3
            x: parent.width / 4
            y: portrait ? (parent.height / 2) - (parent.width / 4) :
                          (parent.height / 3) - (parent.width / 6)
            radius: portrait ? parent.width / 16 : parent.height / 16

            color: Theme.overlayBackgroundColor
            opacity: Theme.opacityOverlay

            Label {
                id: feedbackLabel
                objectName: "feedbackLabel"
                anchors.fill: parent
                anchors.margins: Theme.paddingLarge
                color: Theme.secondaryHighlightColor
                wrapMode: Text.WordWrap
                text: ""
            }
        }

        onClicked: visible = false
    }

    canAccept: (pushoverEmail.text != "") && (pushoverPassword.text != "")

    onAccepted: {
        Lisp.call("cloverlover::pf-login-and-register",
                  pushoverEmail.text, pushoverPassword.text)
    }
}
