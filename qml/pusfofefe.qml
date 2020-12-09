import QtQuick 2.0
import Sailfish.Silica 1.0
import "pages/" as Pages

ApplicationWindow
{
    initialPage: Component { Pages.FirstPage { } }
    cover: Qt.resolvedUrl("cover/CoverPage.qml")
    allowedOrientations: defaultAllowedOrientations
}
