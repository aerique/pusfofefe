(defpackage :app
  (:use :cl :eql)
  (:export #:start-slynk
           #:stop-slynk
           #:start
           #:reload-qml))
(in-package :app)

(qrequire :quick)

(defun sym (symbol package)
  (intern (symbol-name symbol) package))

#+app-repl
(defun start-slynk ()
  (unless (find-package :slynk)
    (require :ecl-quicklisp)
    (funcall (sym 'quickload :ql) :slynk))
  (funcall (sym 'create-server :slynk)
           :port 4005 :dont-close t :style :spawn))
           ;; NOTE: security issue, anyone can connect!
           ;:interface "0.0.0.0" :port 4005 :dont-close t :style :spawn))

#+app-repl
(defun stop-slynk ()
  (when (find-package :slynk)
    (funcall (sym 'stop-server :slynk) 4005)))

(defun start ()
  #+app-repl (start-slynk)
  (cloverlover::read-config)
  (cloverlover::read-messages)
  (cloverlover::set-messages-model)
  (qconnect qml:*quick-view* "statusChanged(QQuickView::Status)"
            (lambda (status)
              (case status
                (#.|QQuickView.Ready|
                   (qml-reloaded))))))

(defun reload-qml (&optional (url "http://localhost:8000/"))
  "Reload QML file from an url, directly on the device."
  (qrun*
   (let ((src (|toString| (|source| qml:*quick-view*))))
     (if (x:starts-with (concatenate 'string "file://" qml:*root*) src)
         (|setSource| qml:*quick-view* (qnew "QUrl(QString)" (x:string-substitute url (concatenate 'string "file://" qml:*root*) src)))
       (qml:reload))
     (|toString| (|source| qml:*quick-view*)))))

(defun qml-reloaded ()
  ;; re-ini
  )

(qlater #'start)
