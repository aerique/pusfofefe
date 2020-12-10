import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    SilicaFlickable {
        width: parent.width
        contentHeight: column.height

        anchors.fill: parent

        PullDownMenu {
            MenuItem {
                text: qsTr("About")
                onClicked: pageStack.push(Qt.resolvedUrl("AboutPage.qml"))
            }
        }

        PageHeader {
            id: pageHeader
            title: qsTr("Settings")
        }

        Column {
            id: column
            width: parent.width
            //height: childrenRect.height

            anchors
            {
                top: pageHeader.bottom
                topMargin: Theme.paddingLarge
            }

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
                EnterKey.onClicked: Lisp.call("cloverlover::login-and-register",
                                              pushoverEmail.text,
                                              pushoverPassword.text)
            }

            Button {
                id: submitCredentials
                text: "Login to Pushover"
                onClicked: Lisp.call("cloverlover::login-and-register",
                                     pushoverEmail.text,
                                     pushoverPassword.text)
            }
        }

        VerticalScrollDecorator {}
    }
}
