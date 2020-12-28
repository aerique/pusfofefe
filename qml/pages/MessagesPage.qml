import QtQuick 2.0
import Sailfish.Silica 1.0
import Nemo.Notifications 1.0
import EQL5 1.0

Page {
    SilicaListView {
        anchors.fill: parent
        model: messagesModel

        header: PageHeader { title: "Messages" }

        delegate: ListItem {
            id: listEntry

            Label {
                id: label
                // XXX not needed?
                //color: listEntry.highlighted ? Theme.highlightColor : Theme.primaryColor
                // No idea why this works and the right anchors below don't.
                width: parent.width - (2 * Theme.paddingLarge)
                truncationMode: TruncationMode.Fade
                maximumLineCount: 1
                text: modelData

                anchors {
                    left: parent.left
                    leftMargin: Theme.paddingLarge
                    // See `width` above.
                    //right: parent.right
                    //rightMargin: Theme.paddingLarge
                    verticalCenter: parent.verticalCenter
                }
            }

            // FIXME can't get this to appear
            //Separator {
            //    horizontalAlignment: Qt.AlignHCenter
            //    anchors.top: label.bottom
            //}

            onClicked: pageStack.push(Qt.resolvedUrl("MessagePage.qml"))

            menu: ContextMenu {
                MenuItem {
                    text: "Delete"
                    // FIXME having two remorses running gives an error
                    // FIXME scrolling the remorse out of view causes it too
                    onClicked: listEntry.remorseAction("Deleting message",
                        function() {
                            Lisp.call("cloverlover::pf-delete-message", index)
                        })
                }
            }
        }

        // XXX This should become a Component?
        RemorsePopup { id: remorse }

        // XXX This should become a Component I guess?  Calling this from the
        // SettingsPage also works, while it is only defined here.
        Notification {
            // FIXME rename `id` and `objectName` to error_notification
            id: notification
            objectName: "notification"  // so it can be called from Lisp
            summary: "Pusfofefe Error"
        }

        PullDownMenu {
            MenuItem {
                text: "Settings"
                onClicked: pageStack.push(Qt.resolvedUrl("SettingsPage.qml"))
            }

            MenuItem {
                text: "Clear All Messages"
                onClicked: remorse.execute("Deleting all messages",
                    function() { Lisp.call("cloverlover::clear-messages") })
            }

            MenuItem {
                text: "Refresh"
                onClicked: Lisp.call("cloverlover::pf-download-messages")
            }
        }

        VerticalScrollDecorator {}
    }
}
