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

            Label {
                width: parent.width
                x: Theme.horizontalPageMargin
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                textFormat: Text.RichText
                onLinkActivated: Qt.openUrlExternally(link)
                text: qsTr("<style>a:link { color: " + Theme.primaryColor + "; }</style>" +
                           "(To sign up for Pushover please go to: " +
                           "<a href='https://pushover.net/signup'>https://pushover.net/signup</a>.)")
            }

            SectionHeader {
                text: "App"
            }

            TextField {
                id: refreshTime
                inputMethodHints: Qt.ImhDigitsOnly
                width: parent.width
                label: "minutes between Pushover checks"
                // 1m < t < 1d
                validator: IntValidator { bottom: 1; top: 1440 }
                text: Lisp.call("cloverlover::get-pushover-refresh")

                EnterKey.iconSource: "image://theme/icon-m-accept"
                EnterKey.onClicked: Lisp.call(
                    "cloverlover::set-pushover-refresh", refreshTime.text)
            }
        }
    }
}
