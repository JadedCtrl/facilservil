(defsystem "facilservil"
  :version "0.2"
  :author "Jaidyn Lev <jadedctrl@teknik.io>"
  :license "CC0"
  :depends-on ("usocket" "trivial-utf-8" "alexandria" "nih")
  :components ((:module "src"
                :components
		((:file "package")
		(:file "meta")
		(:file "io")
		(:file "ex")
		(:file "client")
		(:file "server"))))
		
  :description
	"Simple TCP/telnet server for Lisp.")
