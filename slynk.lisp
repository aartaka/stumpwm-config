(in-package :stumpwm-user)

;; Command to start Slynk server.
(defcommand start-slynk (port) ((:number "The port to open the slynk at "))
  (handler-case
      (slynk:create-server :port port :dont-close t)
    (error ()
      (message "Something's wrong, aborting"))))

(start-slynk 4012)
