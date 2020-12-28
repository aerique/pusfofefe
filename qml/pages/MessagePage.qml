import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    id: page

    SilicaFlickable {
        anchors.fill: parent

        PullDownMenu {
            MenuItem {
                text: "Delete"
                onClicked: remorse.execute(
                    "Deleting message...",
                    function() { console.log("Delete message clicked!") })
            }
        }

        PageHeader {
            id: pageHeader
            title: "Message"
        }

        Label {
            anchors.top: pageHeader.bottom
            text: "message text"
        }

        VerticalScrollDecorator {}
    }
}
