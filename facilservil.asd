(defsystem "facilservil"
  :version "1.0"
  :license "BSD 3-Clause"
  :author "Jaidyn Lev <jadedctrl@posteo.at>"
  :depends-on ("usocket" "flexi-streams" "bordeaux-threads")
  :components ((:file "facilservil"))
  :description
	"Simple TCP/telnet server for Lisp.")
