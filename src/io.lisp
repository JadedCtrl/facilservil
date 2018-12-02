(in-package :facilservil)


;; SOCKET --> NIL
(defun client-read (socket)
  "Read new input from a client socket to its `stack` list of bytes."

  (let* ((socket-id   (make-socket-id socket))
	 (client-id   (gethash socket-id *socket-pairs*))
	 (input-id    (make-client-input-id client-id))
	 (input-stack (gethash input-id *client-data*)))

    (setf (gethash input-id *client-data*)
	  (concatenate 'list input-stack
		       (list (read-byte (usocket:socket-stream socket)))))))



;; SOCKET LIST/ARRAY --> NIL
(defun client-write-bytes (socket bytes)
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
(defun client-write (socket string &optional (line-break nil))
  "Writes a string to a client socket-- w/o line-break, by default."

  (client-write-bytes
    socket
    (trivial-utf-8:string-to-utf-8-bytes
      (if line-break
	(format nil "~A~%" string)
	string))))


;; SOCKET --> NIl
(defun client-input-flush (socket)
  "Clean all input from a socket."

  (let* ((socket-id   (make-socket-id socket))
	 (client-id   (gethash socket-id *socket-pairs*))
	 (input-id    (make-client-input-id client-id)))

    (setf (gethash input-id *client-data*) '())))


;; STRING BOOLEAN --> NIL
(defun client-broadcast (string &optional (line-break nil))
  "Writes a string to all client sockets-- w/o line-break, by default."

  (mapcar
    (lambda (socket) (client-write socket string line-break))
    *client-socket-list*))


;; SOCKET --> STRING
(defun client-input-string (socket)
  "Get input from a client as a string."

  (let* ((socket-id (make-socket-id socket))
	 (client-id (gethash socket-id *socket-pairs*))
	 (input-id (make-client-input-id client-id))
	 (input-bytes (gethash input-id *client-data*))
	 (sanitized-bytes (remove-newline-bytes input-bytes)))

    (ignore-errors (trivial-utf-8:utf-8-bytes-to-string sanitized-bytes))))


 
;; -------------------------------------
 
 
 
(defun remove-newline-bytes (bytes)
  (remove 13 (remove 10 bytes)))

(defun commandp (socket)
  (let* ((socket-id (make-socket-id socket))
	 (client-id (gethash socket-id *socket-pairs*))
	 (input-id (make-client-input-id client-id))
	 (input-bytes (gethash input-id *client-data*))
	 (last-byte (car (last input-bytes))))

    (eq *command-byte* last-byte)))
