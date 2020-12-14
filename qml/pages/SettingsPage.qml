import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        PullDownMenu {
            MenuItem {
                text: "About"
                onClicked: pageStack.push(Qt.resolvedUrl("AboutPage.qml"))
            }
        }

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader {
                id: pageHeader
                title: "Settings"
            }

            SectionHeader {
                text: "Pushover"
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
                x: Theme.horizontalPageMargin
                text: "Login to Pushover"
                onClicked: Lisp.call("cloverlover::login-and-register",
                                     pushoverEmail.text,
                                     pushoverPassword.text)
            }
        }
    }
}
