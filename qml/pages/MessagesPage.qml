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

            Column {
                anchors.fill: parent
                width: parent.width

                Label {
                //TextField {
                    color: listEntry.highlighted ? Theme.highlightColor : Theme.primaryColor
                    x: Theme.horizontalPageMargin
                    width: parent.width - (2 * Theme.horizontalPageMargin)
                    //truncationMode: TruncationMode.Fade
                    //readOnly: true
                    maximumLineCount: 1
                    text: modelData

                    anchors {
                        left: parent.left
                        right: parent.right
                        leftMargin: Theme.paddingMedium
                        rightMargin: Theme.paddingMedium
                        //verticalCenter: parent.verticalCenter
                    }
                }

                Separator {}
            }

            onClicked: pageStack.push(Qt.resolvedUrl("MessagePage.qml"))

            menu: ContextMenu {
                MenuItem {
                    text: "Delete"
                    onClicked: listEntry.remorseAction("Deleting message...", function() { console.log("Deleting") })
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
                onClicked: remorse.execute(
                    "Deleting all messages",
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
