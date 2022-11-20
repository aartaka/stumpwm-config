(in-package :stumpwm-user)

(defun battery-accessible-p ()
  (not (member (battery-portable::battery-info-string)
               '("(not implemented)" "(no battery info)") :test #'equalp)))

(defcommand battery-info-message () ()
  (message (battery-portable::battery-info-string)))

(defvar *battery-warning-thread*
  (bt:make-thread
   (lambda ()
     (loop while t
           do (sleep 20)
           when (and (battery-accessible-p)
                     (< (parse-integer (remove-if-not #'digit-char-p (battery-portable::battery-info-string)))
                        10))
             do (set-border-color "red")
             and do (set-msg-border-width 10)
             and do (message "Battery low!")
           else
             do (set-border-color "white")
             and do (set-msg-border-width 1)
           do (stumpwm::sync-all-frame-windows (current-group))))))

(define-key *top-map* (kbd "s-B") "battery-info-message")
