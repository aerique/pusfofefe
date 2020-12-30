import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    property int messageIndex: -1

    RemorsePopup { id: remorse }

    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        PullDownMenu {
            MenuItem {
                text: "Delete"
                onClicked: remorse.execute("Deleting message",
                    function() {
                        Lisp.call("cloverlover::pf-delete-message",
                                  messageIndex)
                    })
            }
        }

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            // Shows the message index for Pusfofefe and has nothing to do
            // with Pushover.  Mainly for development for checking things.
            // Could be removed.
            PageHeader { title: "Message " + messageIndex }

            DetailItem {
                width: parent.width
                label: "App"
                value: Lisp.call("cloverlover::get-message-app", messageIndex)
            }

            DetailItem {
                width: parent.width
                label: "ID"
                value: Lisp.call("cloverlover::get-message-id", messageIndex)
            }

            DetailItem {
                width: parent.width
                label: "Date"
                value: Lisp.call("cloverlover::get-message-date", messageIndex)
            }

            Label {
                anchors {
                    left: parent.left
                    leftMargin: Theme.paddingLarge
                }
                width: parent.width - (2 * Theme.paddingLarge)
                font.pixelSize: Theme.fontSizeSmall
                color: Theme.highlightColor
                wrapMode: Text.WordWrap
                text: Lisp.call("cloverlover::get-message-text", messageIndex)
            }

            // Does not work for IconButton, FFS.
            ButtonLayout {
                Button {
                //IconButton {
                    text: "<"
                    //icon.source: "image://theme/icon-m-back"
                    onClicked: pageStack.replace(
                        Qt.resolvedUrl("MessagePage.qml"),
                        { messageIndex: messageIndex - 1 })
                    // More user-friendly would be to skip to the last
                    // message.
                    enabled: messageIndex > 0
                }

                Button {
                //IconButton {
                    text: ">"
                    //icon.source: "image://theme/icon-m-forward"
                    onClicked: pageStack.replace(
                        Qt.resolvedUrl("MessagePage.qml"),
                        { messageIndex: messageIndex + 1 })
                    // More user-friendly would be to skip to the first
                    // message.
                    // FIXME figure this out
                    //enabled: messageIndex < listView.n_items
                }
            }
        }
    }
}
