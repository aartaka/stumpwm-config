(in-package :stumpwm-user)

(defcommand battery-info-message () ()
  "Show the battery percentage message."
  (message (uiop:frob-substrings (battery-portable::battery-info-string) '("~") "~~")))

(define-key *top-map* (kbd "s-B") "battery-info-message")
