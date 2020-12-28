;;;; cloverlover.lisp

(in-package :cloverlover)

;(use-package :eql5)


;;; Globals

(setf *app-name*    "Pusfofefe")
(setf *app-version* "0.2")

(defparameter *pushover-email*     "")
(defparameter *pushover-password*  "")
(defparameter *pushover-secret*    "")
(defparameter *pushover-device-id* "")
(defparameter *pushover-refresh*   10)

(defparameter *pushover-messages* '())


;;; Model
;;;
;;; https://gitlab.com/eql/EQL5/-/blob/master/examples/M-modules/quick/item-model/list-model.lisp

(defun set-messages-model ()
  (eql:qlet ((data (eql:qvariant-from-value *pushover-messages*
                                            "QStringList")))
    (eql:|setContextProperty| (qml:root-context) "messagesModel" data)))


(defun clear-messages ()
  (setf *pushover-messages* '())
  (write-messages)
  (set-messages-model))


(defun notification-test ()
  (qml:qml-call "notification" "publish"))


;;; Getters
;;;
;;; As much as I hate them (it's a bit of a smell that something hasn't been
;;; thought out right): I see no better solution for now.

(defun get-pushover-email ()
  *pushover-email*)


(defun get-pushover-password ()
  *pushover-password*)


(defun get-pushover-secret ()
  *pushover-secret*)


(defun get-pushover-device-id ()
  *pushover-device-id*)


(defun get-pushover-refresh ()
  *pushover-refresh*)

(defun set-pushover-refresh (refresh-time)
  (setf *pushover-refresh* refresh-time)
  (format t "Pushover refresh time set to ~Dm.~%" refresh-time)
  (write-config))


(defun get-pushover-messages ()
  *pushover-messages*)


;;; Functions

(defun pf-cover-message ()
  (format nil "~D messages" (length *pushover-messages*)))


(defun path-to-config-file ()
  (let* ((dir (directory-namestring (eql:|writableLocation.QStandardPaths|
                                     eql:|QStandardPaths.AppConfigLocation|)))
         (path (mkstr dir "config.lisp")))
    (ensure-directories-exist path)
    path))


(defun read-config ()
  (let ((cfg (path-to-config-file)))
    (format t "Reading config ~S... " cfg)
    (finish-output)
    (if (probe-file cfg)
        (progn (load cfg)
               (format t "Config read.~%"))
        (format t "No config file found.~%" cfg))))


(defun write-config ()
  (let ((cfg (path-to-config-file)))
    (format t "Writing config ~S... " cfg)
    (finish-output)
    (with-open-file (f cfg :direction :output :if-exists :supersede)
      (format f "(in-package :cloverlover)~%~%~
                 (defparameter *pushover-email*     ~S)~%~
                 (defparameter *pushover-password*  ~S)~%~
                 (defparameter *pushover-secret*    ~S)~%~
                 (defparameter *pushover-device-id* ~S)~%~
                 (defparameter *pushover-refresh*   ~D)~%"
              *pushover-email* *pushover-password* *pushover-secret*
              *pushover-device-id* *pushover-refresh*)))
  (format t "Config written.~%"))


(defun path-to-messages-file ()
  (let* ((dir (directory-namestring (eql:|writableLocation.QStandardPaths|
                                     eql:|QStandardPaths.AppConfigLocation|)))
         (path (mkstr dir "messages.lisp")))
    (ensure-directories-exist path)
    path))


(defun read-messages ()
  (let ((msgs (path-to-messages-file)))
    (format t "Reading messages from ~S... " msgs)
    (finish-output)
    (if (probe-file msgs)
        (progn (load msgs)
               (format t "~D messages read.~%" (length *pushover-messages*)))
        (format t "No messages file found.~%" msgs))))


;;; XXX only do this when application is getting closed (or would that take
;;;     too long?)
(defun write-messages ()
  (let ((msgs (path-to-messages-file)))
    (format t "Writing messages to ~S... " msgs)
    (finish-output)
    (with-open-file (f msgs :direction :output :if-exists :supersede)
      (format f "(in-package :cloverlover)~%~%~
                 (defparameter *pushover-messages*~%  '~S)~%"
              *pushover-messages*)))
  (format t "~D messages written.~%" (length *pushover-messages*)))


(defun pf-download-messages ()
  (let ((response (download-messages *pushover-secret* *pushover-device-id*)))
    (if (= 0 (getf response :status))
        (progn (format t "Could not download messages: ~S~%" response)
               (qml:qml-set "notification" "previewBody"
                            (first (getf response :errors)))
               (qml:qml-set "notification" "body"
                            (first (getf response :errors)))
               (qml:qml-call "notification" "publish"))
        (progn (format t "~D message(s) downloaded.~%"
                       (length (getf response :messages)))
               (setf *pushover-messages*
                     (append *pushover-messages*
                             (loop for msg in (getf response :messages)
                                   collect (getf msg :message))))
               (write-messages)
               (set-messages-model)))))


(defun login-and-register (email password)
  (setf *pushover-email*    email
        *pushover-password* password)
  (format t "Logging in with <<~S>> <<~S>>...~%" email password)
  (let ((*print-pretty* nil)
        (response (login email password)))
    (if (= 0 (getf response :status))
        (progn (format t "Could not login: ~S~%" response)
               (qml:qml-set "notification" "previewBody"
                            (first (getf response :errors)))
               (qml:qml-set "notification" "body"
                            (first (getf response :errors)))
               (qml:qml-call "notification" "publish"))
        (progn (format t "Received secret <<~S>>.~%" (getf response :secret))
               (setf *pushover-secret* (getf response :secret))))
    (format t "Registering device...~%")
    (setf response (register-new-device *pushover-secret* *app-name*))
    (if (= 0 (getf response :status))
        (progn (format t "Could not register device: ~S~%" response)
               (qml:qml-set "notification" "previewBody"
                            (first (getf response :errors)))
               (qml:qml-set "notification" "body"
                            (first (getf response :errors)))
               (qml:qml-call "notification" "publish"))
        (progn (format t "Device registered <<~S>>.~%" (getf response :id))
               (setf *pushover-device-id* (getf response :id))
               (write-config)))))
