import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    RemorsePopup { id: remorse }

    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        PullDownMenu {
            MenuItem {
                text: "About"
                onClicked: pageStack.push(Qt.resolvedUrl("AboutPage.qml"))
            }

            MenuItem {
                text: "Reset Settings"
                onClicked: remorse.execute(
                    "Resetting all settings to default",
                    function() { console.log("Reset all settings.") })
            }
        }

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader { title: "Settings" }

            SectionHeader { text: "Pushover" }

            TextField {
                id: pushoverEmail
                width: parent.width
                label: "E-mail Address"
                placeholderText: "Enter E-mail Address"
                text: Lisp.call("cloverlover::get-pushover-email")

                readOnly: true

                onClicked: pageStack.push(Qt.resolvedUrl("LoginDialog.qml"))
            }

            PasswordField {
                id: pushoverPassword
                text: Lisp.call("cloverlover::get-pushover-password")

                readOnly: true

                onClicked: pageStack.push(Qt.resolvedUrl("LoginDialog.qml"))
            }

            Label {
                width: parent.width
                x: Theme.horizontalPageMargin
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                textFormat: Text.RichText
                onLinkActivated: Qt.openUrlExternally(link)
                text: "<style>a:link { color: " + Theme.primaryColor +
                      "; }</style>(To sign up for Pushover please go to: " +
                      "<a href='https://pushover.net/signup'>https://" +
                      "pushover.net/signup</a>.)"
            }

            SectionHeader { text: "General" }

            ComboBox {
                id: combobox
                width: parent.width
                description: "between Pushover checks"
                currentIndex: 2
                menu: ContextMenu {
                    MenuItem { text: "1 minute" }
                    MenuItem { text: "5 minutes" }
                    MenuItem { text: "10 minutes" }
                    MenuItem { text: "1 hour" }
                    MenuItem { text: "4 hours" }
                    //MenuItem { text: "1 day" }
                    onClicked: console.log(">>> " + combobox.value)
                }
            }

            //Slider {
            //    width: parent.width
            //    minimumValue: 1
            //    maximumValue: 240
            //    value: 10
            //    valueText: value
            //    label: "minutes between Pushover checks"
            //    stepSize: 1
            //}
        }
    }
}
