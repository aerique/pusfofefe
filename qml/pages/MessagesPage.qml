import QtQuick 2.0
import Sailfish.Silica 1.0
import Nemo.Notifications 1.0
import EQL5 1.0

Page {
    // XXX This should become a Component?
    RemorsePopup { id: remorse }

    // XXX This should become a Component I guess?  Calling this from the
    // SettingsPage also works, while it is only defined here.
    Notification {
        id: notification
        objectName: "notification"  // so it can be called from Lisp
        summary: "Pusfofefe Error"
    }

    // XXX component?
    BusyLabel {
        id: busy_label
        objectName: "busy_label"
        text: ""
        running: false
    }

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
                // So this doesn't really work, since the UI seems to be
                // blocked by this function.  (Same if I set the busyLabel
                // from Lisp.)
                // Also tried a WorkerScript but then ECL goes mental with
                // `ecl_import_current_thread`.
                onClicked: function() {
                    busy_label.text = "Retrieving new messages"
                    busy_label.running = true
                    Lisp.call("cloverlover::pf-download-messages")
                    busy_label.running = false
                }
            }
            }
        }

        VerticalScrollDecorator {}
    }
}
