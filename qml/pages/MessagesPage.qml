import QtQuick 2.0
import Sailfish.Silica 1.0
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

        RemorsePopup { id: remorse }

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
