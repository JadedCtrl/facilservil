(in-package :facilservil)


;; DATA [STRING] --> STRING
(defun journal (data &optional (name "unnamed"))
  "Print out a piece of data for logging on stdout."
  (format t "~A | ~A~%" (force-string-length name 10)  data))


(defun standard-journaling ())


;; -------------------------------------


;; LIST --> STRING
(defun print-bytes (bytes)
  "Print a list of (UTF-8) bytes as a string to stdout."

  (if bytes
    (format t "~A"
	    (ignore-errors (tu8:utf-8-bytes-to-string bytes)))))


;; STRING NUMBER [STRING} --> STRING
(defun force-string-length (string desired-length &key (prefix "") (suffix " "))
  "Force a string to be of a certain length-- by whatever means necessary!"

  (cond
    ((< (length string) desired-length)
     (nih:min-string-length string desired-length :suffix suffix :prefix prefix))

    ((< desired-length (length string))
     (nih:line-car (nih:max-string-length string desired-length)))

    ((eq (length string) desired-length)
     string)
     (nih:min-string-length string desired-length :suffix suffix)))
