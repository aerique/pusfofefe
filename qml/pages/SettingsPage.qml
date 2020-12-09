import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    PageHeader {
        id: pageHeader
        title: qsTr("Settings")
    }

    SilicaFlickable {
        width: parent.width
        contentHeight: column.height

        anchors {
            top: pageHeader.bottom
            topMargin: Theme.paddingLarge
            left: parent.left
            leftMargin: Theme.paddingLarge
            right: parent.right
            rightMargin: Theme.paddingLarge
            bottom: parent.bottom  // new
        }

        Column {
            id: column
            width: parent.width
            //height: childrenRect.height

            TextField {
                id: pushoverEmail
                inputMethodHints: Qt.ImhNoAutoUppercase
                width: parent.width
                label: "E-mail Address"
                placeholderText: label

                EnterKey.iconSource: "image://theme/icon-m-enter-next"
                EnterKey.onClicked: pushoverPassword.focus = true
            }

            PasswordField {
                id: pushoverPassword
                //width: parent.width

                EnterKey.iconSource: "image://theme/icon-m-enter-next"
                EnterKey.onClicked: Lisp.call("login-and-register",
                                              pushoverEmail.text,
                                              pushoverPassword.text)
            }

            Button {
                id: submitCredentials
                text: "Login to Pushover"
                onClicked: Lisp.call("login-and-register",
                                     pushoverEmail.text,
                                     pushoverPassword.text)
            }
        }

        VerticalScrollDecorator {}
    }
}