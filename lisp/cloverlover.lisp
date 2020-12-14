;;;; cloverlover.lisp

(in-package :cloverlover)

;(use-package :eql5)


;;; Globals

(setf *app-name*    "Pusfofefe")
(setf *app-version* "0.2")

(defparameter *pushover-email*     nil)
(defparameter *pushover-password*  nil)
(defparameter *pushover-secret*    nil)
(defparameter *pushover-device-id* nil)
(defparameter *pushover-refresh*   10)


;;; Getters
;;;
;;; As much as I hate them (it's a bit of a smell that something hasn't been
;;; thought out right): I see no better solution for now.

(defun set-pushover-refresh (refresh-time)
  (setf *pushover-refresh* refresh-time)
  (format t "Pushover refresh time set to ~Dm.~%" refresh-time)
  (write-config))


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
  (format t "Logging in with <<~S>> <<~S>>...~%" email password)
  (let ((json (login email password)))
    (format t ">>> ~S~%" json)
    (format t ">>> ~S~%" (jsown:val json "status"))))
    ;(if (= (jsown:val json "status") 0)
    ;    (format t "Could not login: ~S~%" json)
    ;    (progn (format t "Received secret <<~S>>.~%" (jsown:val json "secret"))
    ;           (setf *pushover-secret* secret)
    ;           (write-config)))))
    ;(format t "Registering device...~%")
    ;(setf device-id (register-new-device secret *app-name*))
    ;(setf *pushover-device-id* device-id)
    ;(format t "Device registered <<~S>>.~%" device-id)
    ;(write-credentials)))
