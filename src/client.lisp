(in-package :facilservil)

;: SOCKET --> STRING
(defun make-socket-id (socket)
  "Create a socket-id from a socket."
  (format nil "~A" socket))

;; NIL --> NUMBER
(defun make-client-id ()
  "Generate a new client ID."
  (random 999999))

;; STRING --> STRING
(defun make-client-input-id (client-id)
  "Make an 'input ID' from a client-id."
  (format nil "~A-input" client-id))

(defun make-client-output-id (client-id)
  "Make an 'output ID' from a client-id."
  (format nil "~A-output" client-id))


;; -------------------------------------


;; SOCKET --> NIL
(defun client-register (socket)
  "Register a new client; add their data to globals, log it, etc."

  (let* ((client-id (make-client-id))
	 (socket-id (make-socket-id socket))
	 (output-id (make-client-output-id client-id))
	 (input-id (make-client-input-id client-id)))

    (setq *socket-list* (concatenate 'list *socket-list* (list socket)))
    (setq *client-socket-list* (concatenate 'list *client-socket-list*
					    (list socket)))

    (setq
      *client-list*
      (concatenate 'list *client-list* (list client-id)))
    (setf
      (gethash client-id *client-pairs*) socket-id)
    (setf
      (gethash socket-id *socket-pairs*) client-id)
    (setf
      (gethash output-id *client-data*) '())
    (setf
      (gethash input-id *client-data*) '())

    (journal (format nil "Client ~A has connected!" client-id) "Connect")))


;; -------------------------------------


(defun client-slaughter (socket)
  "Clean up data from a client, and disconnect their socket."

  (let* ((socket-id (make-socket-id socket))
	 (client-id (gethash socket-id *socket-pairs*))
	 (output-id (make-client-output-id client-id))
	 (input-id (make-client-input-id client-id)))

    (remhash socket-id *socket-pairs*)
    (remhash client-id *client-pairs*)
    (remhash client-id *client-data*)
    (remhash input-id *client-data*)
    (setq *client-list* (delete client-id *client-list*))
    (setq *socket-list* (delete socket *socket-list*))
    (setq *client-socket-list* (delete socket *client-socket-list*))

    (usocket:socket-close socket)
    (journal
      (format nil "Client ~A has disconnected." client-id)
      "Disconnect")))
