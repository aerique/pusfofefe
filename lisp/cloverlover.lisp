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

(defparameter *pushover-messages* '("app" "not" "miss"))


;;; Model
;;;
;;; https://gitlab.com/eql/EQL5/-/blob/master/examples/M-modules/quick/item-model/list-model.lisp

(defun set-my-model ()
  (eql:qlet ((data (eql:qvariant-from-value *pushover-messages*
                                            "QStringList")))
    (eql:|setContextProperty| (qml:root-context) "myModel" data)))
    ;; This doesn't seem to work:
    ;(qml:qml-set (qml:root-context) "myModel" data)))


(defun test-update ()
  (setf *pushover-messages* (append *pushover-messages* (list (format nil "~D" (get-universal-time)))))
  (set-my-model))


(defun clear-messages ()
  (setf *pushover-messages* '("stub message"))
  (set-my-model))


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
      (format f "(defparameter *pushover-email*     ~S)~%~
                 (defparameter *pushover-password*  ~S)~%~
                 (defparameter *pushover-secret*    ~S)~%~
                 (defparameter *pushover-device-id* ~S)~%~
                 (defparameter *pushover-refresh*   ~D)~%"
              *pushover-email* *pushover-password* *pushover-secret*
              *pushover-device-id* *pushover-refresh*)))
  (format t "Config written.~%"))


(defun login-and-register (email password)
  (setf *pushover-email*    email
        *pushover-password* password)
  (format t "[login-and-register] Logging in with <<~S>> <<~S>>...~%"
          email password)
  (let ((*print-pretty* nil)
        (response (login email password)))
    (format t ">>> ~S~%" response)
    (if (= 0 (getf response :status))
        (progn (format t "[login-and-register] Could not login: ~S~%" response)
               (qml:qml-set "notification" "previewBody"
                            (first (getf response :errors)))
               (qml:qml-set "notification" "body"
                            (first (getf response :errors)))
               (qml:qml-call "notification" "publish"))
        (progn (format t "[login-and-register] Received secret <<~S>>.~%"
                       (getf response :secret))
               (setf *pushover-secret* (getf response :secret))
               (write-config)))))
    ;(format t "Registering device...~%")
    ;(setf device-id (register-new-device secret *app-name*))
    ;(setf *pushover-device-id* device-id)
    ;(format t "Device registered <<~S>>.~%" device-id)
    ;(write-credentials)))
