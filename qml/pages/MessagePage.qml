import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Page {
    property int messageIndex: -1
    property int messageCount: -1

    SilicaFlickable {
        anchors.fill: parent
        contentWidth: parent.width
        contentHeight: column.height

        // No delete from this view for now until we figure out a way to access
        // the `slv.count` in the MessagesPage.
        //PullDownMenu {
        //    MenuItem {
        //        text: "Delete"
        //        onClicked: remorse.execute("Deleting message",
        //            function() {
        //                Lisp.call("cloverlover::pf-delete-message",
        //                          messageIndex)
        //                coverMessages =
        //                      Lisp.call("cloverlover::pf-cover-messages")
        //                coverNewMessages =
        //                      Lisp.call("cloverlover::pf-cover-new-messages")
        //            })
        //    }
        //}

        VerticalScrollDecorator {}

        Column {
            id: column
            spacing: Theme.paddingLarge
            width: parent.width

            PageHeader {
                title: Lisp.call("cloverlover::get-message-app", messageIndex)
            }

            //DetailItem {
            //    width: parent.width
            //    label: "App"
            //    value: Lisp.call("cloverlover::get-message-app", messageIndex)
            //}

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
        }
    }

    // Does not work for IconButton, FFS.
    ButtonLayout {
        //width: parent.width - 2 * Theme.horizontalPageMargin
        //x: Theme.horizontalPageMargin
        y: parent.height - childrenRect.height - Theme.paddingLarge

        Button {
        //IconButton {
            text: "<"
            //icon.source: "image://theme/icon-m-back"
            onClicked: pageStack.replace(
                Qt.resolvedUrl("MessagePage.qml"),
                { messageIndex: messageIndex - 1,
                  messageCount: messageCount })
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
                { messageIndex: messageIndex + 1,
                  messageCount: messageCount })
            // More user-friendly would be to skip to the first
            // message.
            enabled: messageIndex < (messageCount - 1)
        }
    }
}
