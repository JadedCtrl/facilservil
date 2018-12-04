(in-package :facilservil)


;; DATA [STRING] --> STRING
(defun journal (data &optional (name "unnamed") (second-name "-"))
  "Print out a piece of data for logging on stdout."
  (format t "~A | ~A | ~A | ~A~%"
	  (string-date (get-universal-time))
	  (force-string-length name 10)
	  (force-string-length second-name 10)
	  data))


(defun standard-journaling ())


;; -------------------------------------


(defun string-date (universal-time)
  (multiple-value-bind (second minute hour day month year)
    (decode-universal-time universal-time)

    (nih:string-combine
      (nih:string-combine year (make-digits month 2) (make-digits day 2)
			  :seperator "-")
      " "
      (nih:string-combine (make-digits hour 2) (make-digits minute 2)
			  (make-digits second 2) :seperator ":"))))


(defun make-digits (string number)
  (nih:min-string-length string number :prefix "0"))


;; LIST --> STRING
(defun print-bytes (bytes)
  "Print a list of (UTF-8) bytes as a string to stdout."

  (if bytes
    (format t "~A"
	    (ignore-errors (babel:octets-to-string bytes :encoding :utf-8)))))


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
