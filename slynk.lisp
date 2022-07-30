(in-package :stumpwm-user)

(defvar *slynk-port* 4012 "The port to start Slynk at. Change in case of collisions.")

;; Command to start Slynk server.
(defcommand start-slynk () ()
  (slynk:create-server :port *slynk-port*
                       :dont-close t))
