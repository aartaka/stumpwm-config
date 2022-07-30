(in-package :stumpwm-user)

(setf *screen-mode-line-format* (concat "^71[^70 %B ^71]^70 " *screen-mode-line-format*))

(defun battery-accessible-p ()
  (not (member (battery-portable::battery-info-string)
               '("(not implemented)" "(no battery info)") :test #'equalp)))

(defcommand battery-info-message () ()
  (message (battery-portable::battery-info-string)))

(define-key *top-map* (kbd "s-B") "battery-info-message")
