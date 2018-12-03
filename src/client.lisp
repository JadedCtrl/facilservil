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
(defun client-data-id (client-id data)
  "Make an 'input ID' from a client-id."
  (format nil "~A-~A" client-id data))


;; -------------------------------------


;; SOCKET --> NUMBER
(defun socket-to-client (socket)
  "Return the client of a socket."
  (cadr (socket-pair socket)))


;; NUMBER --> SOCKET
(defun client-to-socket (client)
  "Return the socket of a client."
  (car (client-pair client)))



;; NUMBER --> LIST
(defun client-pair (client)
  "Return the '(socket client) pair of a client."
  (nih:getf-cadr *socket-client* client))


;; SOCKET --> LIST
(defun socket-pair (socket)
  "Return the '(socket client) pair of a socket."
  (nih:getf-car *socket-client* socket))


;; -------------------------------------


;; NUMBER STRING DATA --> NIL
(defun client-data-set (client data value)
  "Set a piece of a `client`'s `data` to `value`."

  (setf
    (gethash (client-data-id client data) *client-data*)
    value))


;; NUMBER STRING --> ???
(defun client-data-get (client data)
  "Get the value of a client's `data` from *client-data*."
  (gethash (client-data-id client data) *client-data*))


;; NUMBER STRING --> NIL
(defun client-data-rem (client data)
  "Remove a piece of client's `data` from *client-data*."
  (remhash (client-data-id client data) *client-data*))


;; -------------------------------------


;; SOCKET --> NIL
(defun socket-register (socket)
  "Register a new socket; add their data to globals, log it, etc."

  (let* ((client-id (make-client-id)))

    (setq *socket-list* (concatenate 'list *socket-list* (list socket)))

    (setq *socket-client* (concatenate 'list *socket-client*
				       (list (list socket client-id))))
    (setq *csocket-list* (concatenate 'list *csocket-list*
				      (list socket)))
    (setq
      *client-list* (concatenate 'list *client-list*
				 (list client-id)))

    (client-data-set client-id "input" '())

    (journal (format nil "Client ~A has connected!" client-id) "Connect")))


;; -------------------------------------


;; NUMBER --> NIL
(defun client-slaughter (client)
  "Clean up data from client, and disconnect their socket."
  (socket-slaughter (client-to-socket client)))


;; SOCKET --> NIL
(defun socket-slaughter (socket)
  "Clean up data from a client, and disconnect their socket."

  (let* ((client-id (socket-to-client socket)))

    (client-data-rem client-id "input")
    (setq *socket-client* (delete (socket-pair socket) *socket-client*))
    (setq *socket-list* (delete socket *socket-list*))
    (setq *client-list* (delete client-id *client-list*))
    (setq *csocket-list* (delete socket *csocket-list*))

    (usocket:socket-close socket)
    (journal
      (format nil "Client ~A has disconnected." client-id)
      "Disconnect")))
