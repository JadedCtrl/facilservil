(in-package :facilservil)

;; -------------------------------------
;; GLOBAL VARIABLES

(defvar *socket-client* '())

(defvar *client-list* '())
(defvar *client-data* '())

(defvar *socket-list* '())
(defvar *csocket-list* '())



(defun reset-globals ()
  "Set all global variables to default state."
  (setq *socket-client* '()) ;; list correlating client IDs and sockets

  (setq *client-list* '())  ;; list of client (ID numbers)
  (setq *client-data* (make-hash-table :test 'equal))

  (setq *socket-list* '())  ;; all sockets
  (setq *csocket-list* '())) ;; all sockets except for master socket


;; -------------------------------------


;; STRING NUMBER CHARACTER FUNCTION-NAME FUNCTION-NAME FUNCTION-NAME --> NIL
(defun server
  (host port connecting disconnecting input-handler
	&key (command-byte 10) (halting 'halt-ex))

  "Runs the basic server on `host`:`port`, running `connecting` when a new
  client connects, `disconnecting` when one disconnects, and `input-handler`
  when the byte `command-byte` is recieved as input.

  The three user-provided functions must take the following two arguments:
  (socket client-id)

  The exception, `input-handler`, takes three arguments:
  (socket client-id client-input-string)

  Inspired by
  http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp"

  (let* ((master-socket
	   (usocket:socket-listen host port
				  :reuse-address 'T
				  :element-type '(unsigned-byte 8))))
    (reset-globals)
    (setq *socket-list* (list master-socket))

    (unwind-protect
      (loop 
	(loop
	  :for socket
	  :in (usocket:wait-for-input *socket-list* :ready-only 'T)
	  :do
	  (cond

	    ;; if new connection...
	    ((eq socket master-socket)
	     (let ((new-socket (usocket:socket-accept socket)))

	       ;; add data to client-pairs, socket-pairs, client-data vv
	       (socket-register new-socket)

	       ;; execute user-provided #'connecting ^-^
	       (funcall connecting new-socket (socket-to-client new-socket))))


	    ;; ...if functioning old connection...
	    ((listen (usocket:socket-stream socket))
	     (progn (socket-read socket)
		    ;; check if command is complete-- if so, use user-provided
		    ;; input-handler.
		    (let* ((client-id (socket-to-client socket))
			   (client-bytes (client-data-get client-id "input"))
			   (client-input (client-input-string client-id)))

		      ;; if reached *command-byte*, handle and flush input
		      (if (commandp client-bytes command-byte)
			(progn
			  (funcall input-handler socket client-id client-input)
			  (socket-input-flush socket))))))


	    ;; ...if EOF connection or error... </3
	    ('T
	     ;; execute user-provided #'disconnecting ;-;
	     (funcall disconnecting socket (socket-to-client socket))
	     (socket-slaughter socket))))

	;; now, let's write that shit down
	(standard-journaling))

      ;; unwind-protect's cleanup form:
      ;; if error, shut down gracefully.
      (progn
	(funcall halting)
	(server-shutdown)))))



;; -------------------------------------



(defun server-shutdown ()
  "Shut down the server and close all connections."

  (journal "Shutting down server..." "Halt")
  (ignore-errors (mapcar #'usocket:socket-close *socket-list*)))



;; STRING NUMBER 
(defun server-reboot
  (host port connecting disconnecting input-handler command-byte halting)
  "Shut down the server, then start it again."

  (journal "Server stopping..." "Reboot")
  (server-shutdown)

  (journal "Server starting..." "Reboot")
  (server host port connecting disconnecting input-handler
	  :command-byte command-byte
	  :halting halting))
