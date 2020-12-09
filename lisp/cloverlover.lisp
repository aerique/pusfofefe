;;;; cloverlover.lisp

;;; Packages

(use-package :babel)
(use-package :drakma)
(use-package :jsown)


;;; Globals

(defparameter *api-url* "https://api.pushover.net/1/")

(defparameter *app-name*    "Cloverlover Sailfish OS App")
(defparameter *app-version* "0.0.1")

(defparameter *user-agent* (concatenate 'string *app-name* "/" *app-version*
                                      " " (drakma::user-agent-string :drakma)))

(defparameter *pushover-email*     nil)
(defparameter *pushover-password*  nil)
(defparameter *pushover-secret*    nil)
(defparameter *pushover-device-id* nil)


;;; Common Functions

(defun errmsg (&rest args)
  (apply #'format (append (list *error-output*) args))
  (force-output *error-output*))


(defun make-keyword (string)
  (intern (string-upcase string) :keyword))


;; FIXME use this in all the right places
(defun json2plist (json)
  (loop for key in (jsown:keywords json)
        append (list (make-keyword key)
                     (jsown:val json key))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


;;; Functions

(defun login (email password)
  (let* ((response (drakma:http-request (mkstr *api-url* "users/login.json")
                                      :method :POST :user-agent *user-agent*
                                      :parameters `(("email"    . ,email)
                                                    ("password" . ,password))))
         (json (jsown:parse (babel:octets-to-string response))))
    ;(json2plist json)))
    json))


(defun register-new-device (secret name &optional (os "O"))
  (let* ((response (drakma:http-request (mkstr *api-url* "/devices.json")
                                        :method :POST :user-agent *user-agent*
                                        :parameters `(("secret" . ,secret)
                                                      ("name"   . ,name)
                                                      ("os"     . ,os))))
         (json (jsown:parse (babel:octets-to-string response)))
         (device-id (when (jsown:keyp json "id")
                      (jsown:val json "id")))
         (status (when (jsown:keyp json "status")
                   (jsown:val json "status")))
         (request (when (jsown:keyp json "request")
                    (jsown:val json "request"))))
    (if (and device-id status request)
        (list :device-id device-id :status status :request request)
        (progn (errmsg "~S" json)
               nil))))


(defun delete-messages (secret device-id highest-message-id)
  (let* ((response (drakma:http-request (mkstr *api-url* "/devices/" device-id
                                               "/update_highest_message.json")
                     :method :POST :user-agent *user-agent*
                     :parameters `(("secret"  . ,secret)
                                   ("message" . ,(mkstr highest-message-id)))))
         (json (jsown:parse (babel:octets-to-string response))))
    (if (= (jsown:val json "status") 1)
        t
        (progn (errmsg "~S" json)
               nil))))


(defun parse-messages (json)
  (when (jsown:keyp json "messages")
    (loop for msg in (jsown:val json "messages")
          for id       = (jsown:val msg "id")
          for title    = (if (jsown:keyp msg "title")
                             (jsown:val msg "title")
                             *app-name*)
          for message  = (jsown:val msg "message")
          for date     = (jsown:val msg "date")
          for priority = (jsown:val msg "priority")
          for acked    = (jsown:val msg "acked")
          for aid      = (jsown:val msg "aid")
          for app      = (jsown:val msg "app")
          for icon     = (jsown:val msg "icon")
          for umid     = (jsown:val msg "umid")
          collect (list :id id :title title :message message :date date
                        :priority priority :acked acked :aid aid :app app
                        :icon icon :umid umid))))


(defun download-messages (secret device-id)
  (let* ((response (drakma:http-request (mkstr *api-url* "/messages.json")
                                    :method :GET :user-agent *user-agent*
                                    :parameters `(("secret"    . ,secret)
                                                  ("device_id" . ,device-id))))
         (json (jsown:parse (babel:octets-to-string response))))
    (if (= (jsown:val json "status") 1)
        (values (parse-messages json)
                (append '(:obj) (cddr json)))
        (progn (errmsg "~S" json)
               nil))))


(defun catch-up-on-all-messages (secret device-id)
  (loop for msgs = (download-messages secret device-id)
        while msgs
        for highest-id = (highest-message msgs)
        do (format t "Retrieved ~D messages.~%" (length msgs))
           (format t "  - highest id: ~D~%" highest-id)
           (format t "  - deleting messages...~%")
           (finish-output)
           (delete-messages secret device-id highest-id)
           (sleep 1)))


(defun highest-message (messages)
  "Returns the highest message ID in MESSAGES.
  MESSAGES is assumed to be in the format as returned by DOWNLOAD-MESSAGES."
  (loop with highest-id = 0
        for msg in messages
        for id = (getf msg :id)
        for message = (getf msg :message)
        do (when (> id highest-id)
             (setf highest-id id))
        finally (return highest-id)))


;;; Functions for Sailfish App

(defun path-to-config-file ()
  (let* ((dir (directory-namestring (|writableLocation.QStandardPaths|
                                     |QStandardPaths.AppConfigLocation|)))
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
      (format f "(defparameter *pushover-email* ~S)~%~
                 (defparameter *pushover-password* ~S)~%~
                 (defparameter *pushover-secret* ~S)~%~
                 (defparameter *pushover-device-id* ~S)~%"
              *pushover-email* *pushover-password* *pushover-secret*
              *pushover-device-id*)))
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
