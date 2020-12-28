import QtQuick 2.0
import Sailfish.Silica 1.0
import EQL5 1.0

Dialog {
    Column {
        id: column
        spacing: Theme.paddingLarge
        width: parent.width

        DialogHeader {}

        TextField {
            id: pushoverEmail
            inputMethodHints: Qt.ImhNoAutoUppercase
            width: parent.width
            label: "E-mail Address"
            placeholderText: label
            text: Lisp.call("cloverlover::get-pushover-email")

            EnterKey.iconSource: "image://theme/icon-m-enter-next"
            EnterKey.onClicked: pushoverPassword.focus = true
        }

        PasswordField {
            id: pushoverPassword
            text: Lisp.call("cloverlover::get-pushover-password")

            EnterKey.iconSource: "image://theme/icon-m-accept"
            EnterKey.onClicked: accept()
        }
    }

    // User and password fields should not be empty.
    //canAccept: ...

    onDone: {
        if (result = DialogResult.Accepted) {
            Lisp.call("cloverlover::pf-login-and-register",
                      pushoverEmail.text, pushoverPassword.text)
        }
    }
}
