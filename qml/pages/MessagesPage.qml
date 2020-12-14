import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    id: page

    SilicaListView {
        anchors.fill: parent
        //contentWidth: parent.width
        model: 20

        header: PageHeader {
            title: "Messages"
        }

        delegate: BackgroundItem {
            id: delegate

            Label {
                x: Theme.horizontalPageMargin
                text: "Item " + index
                anchors.verticalCenter: parent.verticalCenter
                color: delegate.highlighted ? Theme.highlightColor :
                                              Theme.primaryColor
            }

            onClicked: console.log("Clicked " + index)
        }

        PullDownMenu {
            MenuItem {
                text: "Settings"
                onClicked: pageStack.push(Qt.resolvedUrl("SettingsPage.qml"))
            }

            MenuItem {
                text: "Clear All Messages"
                onClicked: console.log("Clear all messages.")
            }

            MenuItem {
                text: "Refresh"
                onClicked: console.log("Refresh")
            }
        }

        VerticalScrollDecorator {}
    }
}
