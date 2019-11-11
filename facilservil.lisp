;; facilservil.lisp
;; A simple lib for making a TCP server.
;; Based on a server by Trout,
;; https://gist.github.com/traut/6bf71d0da54493e6f22eb3d00671f2a9
;; which is in turn inspired by
; https://gist.github.com/shortsightedsid/71cf34282dfae0dd2528
; https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc
; http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp

(defpackage :facilservil
  (:use :cl)
  (:export :server :ex-server
           :send :recieve
           :dig :bury
           :close-it
           :get-ip
           :socket→con
           :logger))

(in-package :facilservil)

;; —————————————————————————————————————
;; CLASSES

(defclass connection ()
  ((socket :accessor con→socket :initarg :socket)
   (data   :initform (make-hash-table :test #'equal) :initarg :data)))



;; —————————————————————————————————————
;; MACROS

;; LIST-OF-CONNECTIONS CONNECTION FUNCTION FUNCTION → NIL
(defmacro old-activity (all-connections con on-input on-disconnect)
  "Macro for #'server, for handling client activity."
  `(bordeaux-threads:make-thread
     (lambda ()
       (handler-case
         (process-con-activity ,con ,all-connections ,on-input)
         (t (e)
            (logger "Error during processing ~a" e)
            (setf ,all-connections (delete ,con ,all-connections))
            (funcall ,on-disconnect ,con ,all-connections)
            (close-it ,con))))))

  
;; LIST-OF-CONNECTIONS CONNECTION FUNCTION
(defmacro new-connection (all-connections master-con on-connect)
  "Macro for #'server, for handling new connections."
  `(let* ((new-socket
            (usocket:socket-accept (con→socket ,master-con) :element-type 'character))
          (new-con
            (make-instance 'connection :socket new-socket)))
     (logger "New connection from ~A" (get-ip new-con))
     (push new-con ,all-connections)
     (funcall on-connect new-con ,all-connections)))



;; —————————————————————————————————————
;; SERVER

;; STRING NUMBER [:FUNCTION :FUNCTION :FUNCTION] → NIL
(defun server (host port &key (on-connect #'blank) (on-input #'blank)
                    (on-disconnect #'blank) (on-loop #'blank))
  "Starts server on given host at given port; and executes the given functions
   (with connection/connections/input as arguments) according to their triggers.
   This is the function you want to use.
   Look at #'ex-*, the example server, for example of use."
  (let* ((master-socket (usocket:socket-listen host port :backlog 256))
         (master-con    (make-instance 'connection :socket master-socket))
         (all-connections `(,master-con)))
    (loop
      (loop for con in (wait-for-input all-connections)
            do (if (eq con master-con)
                 (new-connection all-connections master-con on-connect)
                 (old-activity all-connections con on-input on-disconnect)))
      (funcall on-loop all-connections))))


;; STRING NUMBER → THREAD
(defun server-in-thread (host port)
  "Run the TCP server in a seperate thread."
  (let ((thread-name (format nil "facilservil")))
    (logger "Starting server in a separate thread:'~a'" thread-name)
    (bordeaux-threads:make-thread
      (lambda () (server host port))
      :name thread-name)))



;; —————————————————————————————————————
;; CONNECTION I/O

(defgeneric send (target message &rest args)
  (:documentation "Send a given message to a target user."))

;; CONNECTION VARYING → NIL
(defmethod send ((con connection) message &rest args)
  (apply 'send (append (list (con→socket con) message) args)))

;; STREAM-USOCKET VARYING → NIL
(defmethod send ((socket usocket::stream-usocket) message &rest args)
  (let ((sstream (usocket:socket-stream socket)))
    (apply 'format (append (list sstream message) args))
;;    (format sstream (format nil (format nil "~A" message)))
    (force-output sstream)))

;; STREAM-SERVER-USOCKET VARYING → NIL
(defmethod send ((s usocket::stream-server-usocket) a &rest d) nil)

;; LIST-OF-SOCKETS/CONNECTIONS VARYING → NIL
(defmethod send ((sockets list) message &rest args)
  (mapcar (lambda (socket)
            (apply 'send (append (list socket message) args))) sockets))

;; —————————————————

(defgeneric recieve (target)
  (:documentation "Recieve a string from a given target."))

;; CONNECTION → STRING
(defmethod recieve ((con connection))
  (recieve (con→socket con)))

;; STREAM-USOCKET → STRING
(defmethod recieve ((socket usocket::stream-usocket))
  (string-sanitize (read-line (usocket:socket-stream socket))))

;; STREAM-SERVER-USOCKET → NIL
(defmethod recieve ((socket usocket::stream-server-usocket)) nil)



;; —————————————————————————————————————
;; CONNECTION STORAGE

;; CONNECTION STRING → VARYING
(defun dig (connection variable)
  "Get the value of a variable from the connection's hashtable."
  (gethash variable (slot-value connection 'data)))

;; CONNECTION STRING VARYING → VARYING
(defun bury (connection variable value)
  "Set the value of a variable in the connection's hashtable."
  (setf (gethash variable (slot-value connection 'data)) value))



;; —————————————————————————————————————
;; CONNECTION MANAGEMENT

;; SOCKET → NIL
(defun process-con-activity (con connection-list on-input)
  "Process client socket that got some activity"
  (let ((message (recieve con)))
    (logger  "~A: ~A" (get-ip con) message)
    (funcall on-input con message connection-list)))

;; —————————————————

(defgeneric close-it (target &optional con-list on-disconnect)
  (:documentation "Shut down a target's connection, forcefully.
                  Run the disconnect function as well."))

;; CONNECTION LIST-OF-CONNECTIONS FUNCTION
(defmethod close-it ((con connection) &optional connection-list on-disconnect)
  (close-it (con→socket con) connection-list on-disconnect))

;; STREAM-USOCKET LIST-OF-CONNECTIONS FUNCTION
(defmethod close-it ((socket usocket:stream-usocket) &optional connection-list on-disconnect)
  (when connection-list (funcall on-disconnect socket connection-list))
  (handler-case
      (usocket:socket-close socket)
    (error (e)
      (logger "Ignoring the error from closing connection: ~a" e)))
  (logger "Connection closed: ~A" socket))

;; —————————————————

;; SOCKET LIST-OF-CONNECTIONS → CONNECTION
(defun socket→con (socket connections)
  "Return the connection— from a list of them— that matches the given socket."
  (loop :for con :in connections
        :if (eq (con→socket con) socket)
        :return con))

;; —————————————————

;; LIST-OF-CONNECTIONS → LIST-OF-READY-CONNECTIONS
(defun wait-for-input (connections)
  "Basically a wrapper around #'usocket:wait-for-input, but for connections
  rather than stream-usocket objects."
  (let ((sockets (mapcar #'con→socket connections)))
    (mapcar (lambda (socket) (socket→con socket connections))
            (usocket:wait-for-input sockets :timeout 10 :ready-only 'T))))



;; —————————————————————————————————————
;; LOGGING, ETC

;; STRING … ARG → NIL
(defun logger (text &rest args)
  "Simple wrapper around format func to simplify logging."
  (apply 'format (append (list t (concatenate 'string text "~%")) args)))



;; —————————————————————————————————————
;; CONNECTION METADATA

(defgeneric server-p (target)
  (:documentation "Return if a given item's the server's connection/socket."))

;; USOCKET → BOOL
(defmethod server-p ((socket usocket::usocket))
  (eq (type-of socket) 'usocket:stream-server-usocket))

;; CONNECTION → BOOL
(defmethod server-p ((con connection))
  (server-p (con→socket con)))

;; —————————————————

(defgeneric get-ip (target)
  (:documentation "Return the IP address of a given socket/connection."))

;; CONNECTION → IP
(defmethod get-ip ((con connection))
  (get-ip (con→socket con)))

;; STREAM-USOCKET → IP
(defmethod get-ip ((socket usocket::stream-usocket))
  (usocket:get-peer-address socket))



;; —————————————————————————————————————
;; MISC

;; STRING → STRING
(defun string-remove-octets (string &rest restricted-octs)
  "Remove characters from a string matching any passed 'restricted' octet."
  (let ((octets (flexi-streams:string-to-octets string :external-format :utf-8)))
    (mapcar (lambda (octet) (setq octets (remove octet octets))) restricted-octs)
    (flexi-streams:octets-to-string octets :external-format :utf-8)))

;; NUMBER NUMBER → LIST
(defun range (start end)
  "Return whole numbers between start and end, inclusive."
  (loop :for i :from start :to end :collect i))

(defun string-sanitize (string)
  (string-remove-octets string 12 13 14 15))

;; VARYING … → NIL
(defun blank (&rest ignored)
  "Literal nothing. Used as a default for #'server, so that one can ommit
  any given trigger, if they want."
  nil)



;; —————————————————————————————————————
;; EXAMPLE SERVER

;; This is a general outline of any server using facilservil.
;; Four functions for each type of trigger (connection, disconnection, input,
;; loop), passed to the #'facilservil:server function.

;; If you can't tell, it's a simple chat server!

;; STRING NUMBER
(defun ex-server (host port)
  "Wrapping up the example-server for convenience."
  (server host port
          :on-connect #'ex-connect :on-disconnect #'ex-disconnect
          :on-input #'ex-input :on-loop #'ex-loop))


;; CONNECTION LIST-OF-CONNECTIONS → NIL
(defun ex-connect (con con-list)
  "Executed whenever a client connects."
  (bury con "id-number" (random 9999))

  (send con "Welcome to facila example! ♥~%")
  (send con "Users online now—~%")
  (mapcar (lambda (acon) (send con "~A, " (dig acon "id-number"))) con-list)
  (send con "~%~%")

  (send con-list
        (format nil "~A just joined as ~A!~%"
                (get-ip con) (dig con "id-number"))))


;; CONNECTION LIST-OF-CONNECTIONS → NIL
(defun ex-disconnect (con con-list)
  "Executed whenever a client disconnects."
  (send con-list (format nil "~A just died~%" (dig con "id-number"))))

;; CONNECTION STRING LIST-OF-CONNECTIONS → NIL
(defun ex-input (con input con-list)
  "Executed on a connection + it's input."
  (send (remove con con-list) "~A: ~A~%" (dig con "id-number") input))

;; LIST-OF-CONNECTIONS → NIL
(defun ex-loop (con-list) 
  "Executed after input taken, or after #'wait-until-input timeout
  (so maximum, every 10 seconds)."
  nil)
