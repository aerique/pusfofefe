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
(defun ends-with (sequence subsequence)
  (let ((seqlen (length sequence))
        (sublen (length subsequence)))
    (when (and (> sublen 0)
               (<= sublen seqlen))
      (equal (subseq sequence (- seqlen sublen)) subsequence))))


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
  ;; This is a bit too much tied to the GUI for my liking.
  (case *pushover-refresh*
    (   60 0)
    (  300 1)
    (  600 2)
    ( 3600 3)
    (14400 4)
    (otherwise -1)))

(defun set-pushover-refresh (refresh-time)
  ;; A bit blunt but I don't want to import CL-PPCRE.
  (let ((value (parse-integer (subseq refresh-time 0
                                      (position #\space refresh-time))))
        (multiplier (cond ((or (ends-with refresh-time "minute")
                               (ends-with refresh-time "minutes"))
                           60)
                          ((or (ends-with refresh-time "hour")
                               (ends-with refresh-time "hours"))
                           (* 60 60))
                          (t (pf-feedback (mkstr "Unknown multiplier: "
                                                 refresh-time))
                             (return-from set-pushover-refresh)))))
    (setf *pushover-refresh* (* value multiplier)))
  (format t "Pushover refresh time set to ~Ds.~%" *pushover-refresh*)
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
             (cond ;; XXX This seems to be an error in Pushover's API: it
                   ;;     returns `(:NAME ("has already been taken"))` when
                   ;;     a device has already been registered.
                   ;;     I still need to double-check with Curl whether the
                   ;;     problem is really Pushover.
                   ((eq message :name)
                    (mkstr "<br>Device already registered. Login to the "
                           "Pushover website and remove it."))
                   ;; invalid credentials
                   ((starts-with message "invalid email and/or password")
                    "<br>E-mail or password incorrect.")
                   ;; *pushover-secret* is empty
                   ((starts-with message "secret must be supplied; did you ")
                    "<br>Login to Pushover first.")
                   ;; pass the original message
                   (t (mkstr "<br>" message)))))
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
               (pf-feedback (first (getf response :errors)))
               (return-from pf-login-and-register))
        (progn (format t "Received secret <<~S>>.~%" (getf response :secret))
               (setf *pushover-secret* (getf response :secret))))
    (format t "Registering device...~%")
    (setf response (register-new-device *pushover-secret* *app-name*))
    (if (= 0 (getf response :status))
        (progn (format t "Could not register device: ~S~%" response)
               (pf-feedback (first (getf response :errors))))
        (progn (format t "Device registered <<~S>>.~%" (getf response :id))
               (setf *pushover-device-id* (getf response :id))
               (write-config)))))
