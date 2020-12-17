import QtQuick 2.0
import Sailfish.Silica 1.0
import Nemo.Notifications 1.0
import EQL5 1.0

Page {
    id: page

    SilicaListView {
        anchors.fill: parent
        model:myModel

        header: PageHeader { title: "Messages" }

        delegate: BackgroundItem {
            id: delegate

            Label {
                x: Theme.horizontalPageMargin
                anchors.verticalCenter: parent.verticalCenter
                color: Theme.primaryColor
                text: modelData
            }

            onClicked: Lisp.call("cloverlover::test-update")
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
                onClicked: console.log("Refresh")
            }
        }

        VerticalScrollDecorator {}
    }
}
