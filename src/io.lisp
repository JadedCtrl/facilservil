(in-package :facilservil)


;; -------------------------------------
;; SOCKET I/O


;; SOCKET --> NIL
(defun socket-read (socket)
  "Read new input from a client socket to its `stack` list of bytes."

  (let* ((client-id   (socket-to-client socket))
	 (input-stack (client-data-get client-id "input")))
    (client-data-set
      client-id "input"
      (concatenate 'list input-stack
		   (list (read-byte (usocket:socket-stream socket)))))))


;; SOCKET LIST/ARRAY --> NIL
(defun socket-write-bytes (socket bytes)
  "Write bytes to a client socket."

  (let ((sstream (usocket:socket-stream socket))
	(i 0))
    (loop
      :while (< i (length bytes))
      :do
      (cond ((listp bytes)
	     (write-byte (nth i bytes) sstream))
	    ((arrayp bytes)
	     (write-byte (aref bytes i) sstream)))
      (setq i (+ i 1)))

    (force-output sstream)))


;; SOCKET STRING [BOOLEAN] --> NIL
(defun socket-write (socket string &optional (line-break nil))
  "Writes a string to a socket-- w/o line-break, by default."

  (socket-write-bytes
    socket
    (ignore-errors
      (babel:string-to-octets
	(if line-break
	  (format nil "~A~%" string)
	  string)
	:encoding :utf-8))))


;; STRING [BOOLEAN] [SOCKET] --> NIL
(defun socket-broadcast (string &optional (line-break nil) (exception nil))
  "Writes a `string` to all client sockets (aside from an `exception`)."

  (client-broadcast string line-break
		    (socket-to-client exception)))


;; SOCKET --> NIL
(defun socket-input-flush (socket)
  "Clean all input from a socket."

  (let ((client-id   (socket-to-client socket)))
    (client-data-set client-id "input" '())))


;; SOCKET --> STRING
(defun socket-input-string (socket)
  "Get input from a socket as a string."

  (client-input-string (socket-to-client socket)))



;; -------------------------------------
;; CLIENT I/O



;; NUMBER --> NIL
(defun client-read (client)
  "Read new input from a client to their stack of input bytes."

  (socket-read (client-to-socket client)))


;; NUMBER LIST --> NIL
(defun client-write-bytes (client bytes)
  "Write bytes to a client's socket."
  (format t "CLIENT-TO-SOCKET: " (client-to-socket client))

  (socket-write-bytes (client-to-socket client) bytes))


;; NUMBER STRING [BOOLEAN] --> NIL
(defun client-write (client string &optional (line-break nil))
  "Writes a string to a client's socket-- w/o line-break, default."

  (socket-write (client-to-socket client) string line-break))


;; NUMBER --> NIL
(defun client-input-flush (client)
  "Clean up input from a client."

  (socket-input-flush (client-to-socket client)))


;; STRING [BOOLEAN] [SOCKET] --> NIL
(defun client-broadcast (string &optional (line-break nil) (exception nil))
  "Writes a `string` to all client sockets (aside from an `exception`-- w/o line-break, by default."

  (mapcar
    (lambda (client)
      (if (not (eq client exception))
	(client-write client string line-break)))
    *client-list*))


;; NUMBER --> STRING
(defun client-input-string (client)
  "Get input from a client as a string."

  (let* ((input-bytes (client-data-get client "input"))
	 (sanitized-bytes (remove-newline-bytes input-bytes))
	 (byte-vector (list-to-byte-vector sanitized-bytes)))

    (if sanitized-bytes
      (ignore-errors (babel:octets-to-string byte-vector :encoding :utf-8))
      "")))



(defun list-to-byte-vector (list)
  (make-array (list (length list))
	      :initial-contents list
	      :element-type '(unsigned-byte 8)))


;; -------------------------------------

;; SOCKET --> BOOLEAN
(defun socket-connectp (socket)
  "Return whether or not a socket is still connected."
  (listen (usocket:socket-stream socket)))

;; -------------------------------------
;; MISC.



;; LIST --> LIST
(defun remove-newline-bytes (bytes)
  "Remove undesired bytes-- null, LF, CR, etc, from a list of bytes."

  (remove 0 (remove 10 (remove 13 bytes))))


;; SOCKET
(defun commandp (byte-list command-byte)
  "Returns whether or not a command is complete, judging on it's bytes."

  (let* ((last-byte (car (last byte-list))))

    (eq command-byte last-byte)))


