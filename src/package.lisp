(defpackage :facilservil
  (:use :cl)
  (:nicknames :fs)
  (:export

    *socket-client*

    *socket-list*
    *csocket-list*

    *client-list*
    *client-data*

    :journal

    :client-write
    :cline-write-bytes
    :client-broadcast
    :client-read
    :client-slaughter

    :socket-write
    :socket-write-bytes
    :socket-broadcast
    :socket-read
    :socket-register
    :socket-slaughter

    :client-data-get
    :client-data-set
    :client-data-rem

    :socket-to-client
    :client-to-socket

    :connect-ex
    :disconnect-ex
    :input-handle-ex
    :halt-ex
    :reboot-ex

    :ex-server
    
    :server-shutdown
    :server-reboot

    :strequal

    :server))

(in-package :facilservil)


(rename-package :trivial-utf-8 :trivial-utf-8 (list :tu8))
