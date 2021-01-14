import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    SilicaListView {
        id: slv
        anchors.fill: parent
        model: messagesModel

        header: PageHeader { title: "Messages" }

        delegate: ListItem {
            id: listEntry

            //contentHeight: Theme.itemSizeMedium

            Label {
                id: label

                anchors {
                    left: parent.left
                    leftMargin: Theme.paddingLarge
                }

                // XXX not needed?
                //color: listEntry.highlighted ? Theme.highlightColor : Theme.primaryColor
                width: parent.width - (2 * Theme.paddingLarge)
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                text: modelData
            }

            Label {
                anchors {
                    top: label.bottom
                    left: parent.left
                    leftMargin: Theme.paddingLarge
                }

                width: parent.width - (2 * Theme.paddingLarge)
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                font.pixelSize: Theme.fontSizeExtraSmall
                text: Lisp.call("cloverlover::get-message-date", index)
            }

            onClicked: pageStack.push(Qt.resolvedUrl("MessagePage.qml"),
                                      { messageIndex: index,
                                        messageCount: slv.count })

            menu: ContextMenu {
                MenuItem {
                    text: "Delete"
                    // FIXME having two remorses running gives an error
                    // FIXME scrolling the remorse out of view causes it too
                    onClicked: listEntry.remorseAction("Deleting message",
                        function() {
                            Lisp.call("cloverlover::pf-delete-message", index)
                            coverMessages =
                                Lisp.call("cloverlover::pf-cover-messages")
                            coverNewMessages =
                                Lisp.call("cloverlover::pf-cover-new-messages")
                        })
                }
            }
        }

        PullDownMenu {
            MenuItem {
                text: "Settings"
                onClicked: pageStack.push(Qt.resolvedUrl("SettingsPage.qml"))
            }

            MenuItem {
                text: "Clear All Messages"
                onClicked: remorse.execute("Deleting all messages",
                    function() { Lisp.call("cloverlover::pf-clear-messages") })
            }

            MenuItem {
                text: "Refresh"
                onClicked: function() {
                    Lisp.call("cloverlover::pf-download-messages")
                    setMessagesModelTimer.running = true
                }
            }
        }

        VerticalScrollDecorator {}

        // This didn't work when directly positioned below `delegate:` ?!
        ViewPlaceholder {
            enabled: slv.count == 0
            text: "No messages"
            hintText: "Pull down to refresh"
        }
    }

    onActiveFocusChanged: {
        Lisp.call("cloverlover::pf-reset-new-messages")
        coverMessages = Lisp.call("cloverlover::pf-cover-messages")
        coverNewMessages = Lisp.call("cloverlover::pf-cover-new-messages")
    }
}
