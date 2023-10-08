(in-package :stumpwm-user)

(defcommand start-slynk (port) ((:number "The port to open the slynk at "))
  "Command to start Slynk server.
Slynk is already enabled by default on port 4012."
  (handler-case
      (slynk:create-server :port port :dont-close t)
    (error ()
      (message "Something's wrong, aborting"))))

(start-slynk 4012)
