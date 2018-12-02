(in-package :facilservil)


(defun connect-ex (socket client-id)
  "Example connection handler-- sends a friendly welcome message!"

  (client-write socket
		(format nil "Hey, welcome to this server, ~A! <3" client-id)
		'T))


(defun disconnect-ex (socket client-id)
  "Example disconnection handler-- tells the world, for some reason."

  (client-broadcast
    (format nil "~%~A's just run away... </3~%" client-id)
    'T))



(defun input-handle-ex (socket client-id input-string)
  "Example input-handler. Reverses user input and sends it back at them!"

  (if input-string
    (let* ((reversed-input
	     (nih:char-string (reverse (nih:char-list input-string))))

	   (output-string
	     (nih:string-combine
	       (format nil "Sorry, I didn't hear that quite right.~%")
	       (format nil "Did you say, \"~A?\"" reversed-input))))

      (client-write socket output-string 'T))))


(defun halt-ex ()
  "Example (and default) halt-handler-- send halt messages to everyone."

  (journal "Sending halt messages..." "Halt")
  (ignore-errors (client-broadcast
		   (format nil "~%Server shutting down! </3~%"))))


(defun reboot-ex ()
  "Example (and default) reboot-handler-- send reboot messages to everyone."

  (journal "Sending reboot messages..." "Reboot")
  (ignore-errors (client-broadcast
		   (format nil "~%Server restarting now! ^-^~%"))))


;; -------------------------------------


;; NUMBER --> NIL
(defun ex-server (port)
  "Start a server-instance with the built-in example functions.
  Demonstration purposes only!"

  (server "0.0.0.0" port 'connect-ex 'disconnect-ex 'input-handle-ex))
