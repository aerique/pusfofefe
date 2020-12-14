import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    id: page

    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width

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

        SilicaListView {
            anchors.fill: parent
            model: 20

            PageHeader {
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
        }
    }
}
