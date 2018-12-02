(defpackage :facilservil
  (:use :cl)
  (:export

    *socket-list*
    *socket-pairs*

    *client-list*
    *client-pairs*
    *client-data*
    *client-socket-list*

    :journal

    :client-write
    :cline-write-bytes
    :client-broadcast
    :client-slaughter

    :connect-ex
    :disconnect-ex
    :input-handle-ex
    :halt-ex
    :reboot-ex

    :ex-server
    
    :server-shutdown
    :server-reboot

    :server))

(in-package :facilservil)
