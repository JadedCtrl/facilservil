(in-package :facilservil)

;; -------------------------------------
;; GLOAL VARIABLES

(defvar *socket-list* '())
(defvar *socket-pairs* '())

(defvar *client-list* '())
(defvar *client-socket-list* '())
(defvar *client-pairs* '())
(defvar *client-data* '())

(defvar *command-byte* 10)


(defun reset-globals ()
  "Set all global variables to default state."
  (setq *socket-pairs* (make-hash-table :test 'equal))
  (setq *client-list* '())
  (setq *client-socket-list* '())
  (setq *client-pairs* (make-hash-table :test 'equal))
  (setq *client-data* (make-hash-table :test 'equal))
  (setq *command-byte* 10))


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
    (setq *command-byte* command-byte)
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
	       (client-register new-socket) 

	       ;; execute user-provided #'connecting ^-^
	       (let* ((socket-id (make-socket-id new-socket))
		      (client-id (gethash socket-id *socket-pairs*)))
		 (funcall connecting new-socket client-id))))


	    ;; ...if functioning old connection...
	    ((listen (usocket:socket-stream socket))
	     (progn (client-read socket)
		    ;; check if command is complete-- if so, use user-provided
		    ;; input-handler.
		    (let* ((socket-id (make-socket-id socket))
			   (client-id (gethash socket-id *socket-pairs*))
			   (client-input (client-input-string socket)))

		      ;; if reached *command-byte*, handle and flush input
		      (if (commandp socket)
			(progn (funcall input-handler socket client-id client-input)
			       (client-input-flush socket))))))


	    ;; ...if EOF connection or error... </3
	    ('T
	     (let* ((socket-id (make-socket-id socket))
		    (client-id (gethash socket-id *socket-pairs*)))

	       ;; execute user-provided #'disconnecting ;-;
	       (funcall disconnecting socket client-id)
	       (client-slaughter socket))))

	  ;; now, let's write that shit down
	  (standard-journaling)))

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
