;;;; cloverlover.lisp

(in-package :cloverlover)

;(use-package :eql5)


;;; Globals

(setf *app-name*    "Pusfofefe")
(setf *app-version* "0.3")

(defparameter *pushover-email*     "")
(defparameter *pushover-password*  "")
(defparameter *pushover-secret*    "")
(defparameter *pushover-device-id* "")
(defparameter *pushover-refresh*   10)

(defparameter *pushover-response* nil)
(defparameter *pushover-messages* '())
(defparameter *pushover-messages-internal* '())


;;; Move to cloverlover project
;;; BEGIN
(defun starts-with (sequence subsequence)
  (let ((sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen (length sequence)))
      (equal (subseq sequence 0 sublen) subsequence))))


;; https://en.wikipedia.org/wiki/Unix_time
(defvar +unix-epoch-as-universal-time+
        (encode-universal-time 0 0 0 1 1 1970 0))


(defun universal-time-to-ymdhms (universal-time)
  (multiple-value-bind (sec min hr day mon year)
      (decode-universal-time universal-time)
    (format nil "~2,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year mon day hr min sec)))


(defun unix-time-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-as-universal-time+))


(defun unix-time-to-ymdhms (unix-time)
  (universal-time-to-ymdhms (unix-time-to-universal-time unix-time)))
;;; END


;;; Model
;;;
;;; https://gitlab.com/eql/EQL5/-/blob/master/examples/M-modules/quick/item-model/list-model.lisp

(defun set-messages-model ()
  ;; sort messages by descending date
  (setf *pushover-messages-internal* (sort *pushover-messages-internal* #'>
                                           :key (lambda (x) (getf x :date))))
  (setf *pushover-messages* (loop for msg in *pushover-messages-internal*
                                  collect (getf msg :message)))
  (eql:qlet ((data (eql:qvariant-from-value *pushover-messages*
                                            "QStringList")))
    (eql:|setContextProperty| (qml:root-context) "messagesModel" data)))


(defun notification-test ()
  (qml:qml-call "notification" "publish"))


;;; Getters
;;;
;;; As much as I hate them (it's a bit of a smell that something hasn't been
;;; thought out right): I see no better solution for now.

(defun get-message-app (index)
  (if (and (numberp index)
           (>= index 0)
           (< index (length *pushover-messages-internal*)))
      (getf (nth index *pushover-messages-internal*) :app)
      (format nil "Invalid message index: <<~S>>~%" index)))


(defun get-message-date (index)
  (if (and (numberp index)
           (>= index 0)
           (< index (length *pushover-messages-internal*)))
      (let ((unix-time (getf (nth index *pushover-messages-internal*) :date)))
        (if (integerp unix-time)
            (universal-time-to-ymdhms (unix-time-to-universal-time unix-time))
            (format nil "Invalid time: <<~S>>~%" unix-time)))
      (format nil "Invalid message index: <<~S>>~%" index)))


(defun get-message-id (index)
  (if (and (numberp index)
           (>= index 0)
           (< index (length *pushover-messages-internal*)))
      (getf (nth index *pushover-messages-internal*) :id)
      (format nil "Invalid message index: <<~S>>~%" index)))


(defun get-message-text (index)
  (if (and (numberp index)
           (>= index 0)
           (< index (length *pushover-messages-internal*)))
      (getf (nth index *pushover-messages-internal*) :message)
      (format nil "Invalid message index: <<~S>>~%" index)))


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


;;; Functions

(defun find-message (id)
  (loop for msg in *pushover-messages-internal*
        do (when (= id (getf msg :id))
             ;(format t "Message found: ~S~%" msg)
             (return-from find-message msg))))


(defun path-to-config-file ()
  (let* ((dir (directory-namestring (eql:|writableLocation.QStandardPaths|
                                     eql:|QStandardPaths.AppConfigLocation|)))
         (path (mkstr dir "config.lisp")))
    (ensure-directories-exist path)
    path))


(defun path-to-messages-file ()
  (let* ((dir (directory-namestring (eql:|writableLocation.QStandardPaths|
                                     eql:|QStandardPaths.AppConfigLocation|)))
         (path (mkstr dir "messages.lisp")))
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


(defun read-messages ()
  (let ((msgs (path-to-messages-file)))
    (format t "Reading messages from ~S... " msgs)
    (finish-output)
    (if (probe-file msgs)
        (progn (load msgs)
               (format t "~D messages read.~%"
                       (length *pushover-messages-internal*))
               (set-messages-model))
        (format t "No messages file found.~%" msgs))))


;;; XXX only do this when application is getting closed (or would that take
;;;     too long?)
(defun write-messages ()
  (let ((msgs (path-to-messages-file)))
    (format t "Writing messages to ~S... " msgs)
    (finish-output)
    (with-open-file (f msgs :direction :output :if-exists :supersede)
      (format f "(in-package :cloverlover)~%~%~
                 (defparameter *pushover-messages-internal*~%  '~S)~%"
              *pushover-messages-internal*)))
  (format t "~D messages written.~%" (length *pushover-messages-internal*)))


;;; Pusfofefe Functions
;;;
;;; These are called from QML.

(defun pf-feedback (message)
  (let ((msg ; Some Pushover API messages aren't really useful for app users.
             (cond ;; *pushover-secret* is empty.
                   ((starts-with message "secret must be supplied; did you ")
                    "You need to login to Pushover first. (NO_SECRET)")
                   ;; Pass the original message through.
                   (t message))))
    (qml:qml-set "feedbackLabel" "text" msg))
  (qml:qml-set "feedback" "visible" t))


(defun pf-cover-message ()
  (format nil "~D messages" (length *pushover-messages-internal*)))


(defun pf-clear-messages ()
  (setf *pushover-messages-internal* '())
  (write-messages)
  (set-messages-model))


(defun pf-delete-message (index)
  (setf *pushover-messages-internal*
        (remove (nth index *pushover-messages-internal*)
                *pushover-messages-internal*))
  (write-messages)
  (set-messages-model))


(defun pf-download-messages ()
  ;(qml:qml-set "busy_label" "text" "Retrieving new messages")
  ;(qml:qml-set "busy_label" "running" t)
  (let ((response (download-messages *pushover-secret* *pushover-device-id*)))
    (if (= 0 (getf response :status))
        (progn (format t "Could not download messages: ~S~%" response)
               (pf-feedback (first (getf response :errors))))
        (progn (format t "~D message(s) downloaded.~%"
                       (length (getf response :messages)))
               (setf *pushover-response* response
                     *pushover-messages-internal*
                     (append *pushover-messages-internal*
                             (loop for msg in (getf response :messages)
                                   when (not (find-message (getf msg :id)))
                                     collect msg)))
               (write-messages)
               (set-messages-model)
               (if (> (length (getf response :messages)) 0)
                   (progn (delete-messages *pushover-secret*
                               *pushover-device-id*
                               (highest-message *pushover-messages-internal*))
                          (format t "Messages up to ~D deleted from server.~%"
                               (highest-message *pushover-messages-internal*)))
                   (format t "No new messages retrieved, nothing to delete ~
                              from server.~%"))))))
  ;(qml:qml-set "busy_label" "running" nil))


(defun pf-login-and-register (email password)
  (setf *pushover-email*    email
        *pushover-password* password)
  (format t "Logging in with <<~S>> <<~S>>...~%" email password)
  (let ((*print-pretty* nil)
        (response (login email password)))
    (if (= 0 (getf response :status))
        (progn (format t "Could not login: ~S~%" response)
               (pf-feedback (format nil "Could not login: ~S~%" response)))
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
