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

            MenuItem {
                text: "Reset Settings"
                onClicked: remorse.execute(
                    "Resetting all settings to default",
                    function() { Lisp.call("cloverlover::pf-reset-settings") })
            }
        }

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader { title: "Settings" }

            SectionHeader { text: "Pushover" }

            BackgroundItem {
                width: parent.width
                height: pushoverEmail.height + pushoverPassword.height

                TextField {
                    id: pushoverEmail
                    objectName: "pushoverEmail"
                    width: parent.width
                    placeholderText: label
                    label: "E-mail Address"
                    text: Lisp.call("cloverlover::get-pushover-email")
                }

                PasswordField {
                    id: pushoverPassword
                    objectName: "pushoverPassword"
                    // Since we're a child of BackgroundItem and not Column
                    // we need to use an anchor again.
                    anchors.top: pushoverEmail.bottom
                    text: Lisp.call("cloverlover::get-pushover-password")
                }

                //TouchBlocker { anchors.fill: parent }
                BackgroundItem {
                    anchors.fill: parent
                    onClicked: pageStack.push(
                        Qt.resolvedUrl("LoginDialog.qml"))
                }

                //onClicked: pageStack.push(Qt.resolvedUrl("LoginDialog.qml"))
            }

            Label {
                width: parent.width - 2 * Theme.horizontalPageMargin
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
                id: pushoverRefresh
                objectName: "pushoverRefresh"
                width: parent.width
                description: "between Pushover checks"
                currentIndex: Lisp.call(
                    "cloverlover::get-pushover-refresh-for-combobox")
                menu: ContextMenu {
                    MenuItem { text: "1 minute" }
                    MenuItem { text: "5 minutes" }
                    MenuItem { text: "10 minutes" }
                    MenuItem { text: "1 hour" }
                    MenuItem { text: "4 hours" }
                    //MenuItem { text: "1 day" }
                    onClicked: Lisp.call("cloverlover::set-pushover-refresh",
                                         pushoverRefresh.value)
                }
            }
        }
    }
}
