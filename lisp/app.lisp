(defpackage :app
  (:use :cl :eql :qml)
  (:export #:start-slynk
           #:stop-slynk
           #:start
           #:reload-qml))
(in-package :app)

(qrequire :quick)

(defun sym (symbol package)
  (intern (symbol-name symbol) package))

#+app-debug
(defun start-slynk ()
  (unless (find-package :slynk)
    (require :ecl-quicklisp)
    (funcall (sym 'quickload :ql) :slynk))
  (funcall (sym 'create-server :slynk)
           :port 4005 :dont-close t :style :spawn))
           ;; NOTE: security issue, anyone can connect!
           ;:interface "0.0.0.0" :port 4005 :dont-close t :style :spawn))

#+app-debug
(defun stop-slynk ()
  (when (find-package :slynk)
    (funcall (sym 'stop-server :slynk) 4005)))

(defun start ()
  #+app-debug (start-slynk)
  (eql-user::read-config)
  (ini-quick-view (main-qml))
  (qconnect (qview) "statusChanged(QQuickView::Status)"
            (lambda (status)
              (case status
                (#.|QQuickView.Ready|
                   (qml-reloaded))
                (#.|QQuickView.Error|
                   (qmsg (x:join (mapcar '|toString| (|errors| (qview)))
                                 #.(make-string 2 :initial-element #\Newline)))))))
  (qexec))

(defun reload-qml (&optional (url "http://localhost:8000/"))
  "Reload QML file from an url, directly on the device."
  (qrun*
   (let ((src (|toString| (|source| (qview)))))
     (if (x:starts-with (concatenate 'string "file://" (path-to "")) src)
         (|setSource| (qview) (qnew "QUrl(QString)" (x:string-substitute url (concatenate 'string "file://" (path-to "")) src)))
       (reload))
     (|toString| (|source| (qview))))))

(defun qml-reloaded ()
  ;; re-ini
  )
