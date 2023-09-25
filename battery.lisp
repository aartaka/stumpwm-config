(in-package :stumpwm-user)

(defun battery-accessible-p ()
  (not (member (battery-portable::battery-info-string)
               '("(not implemented)" "(no battery info)") :test #'equalp)))

(defcommand battery-info-message () ()
  (message (ppcre:regex-replace-all "~" (battery-portable::battery-info-string) "~~")))

(defvar *battery-warning-thread*
  (bt:make-thread
   (lambda ()
     (loop while t
           do (sleep 20)
           when (and (battery-accessible-p)
                     (< (parse-integer (first (ppcre:all-matches-as-strings
                                               "\\d{1,3}%" (battery-portable::battery-info-string)))
                                       :junk-allowed t)
                        10))
             do (set-border-color "red")
             and do (set-fg-color "red")
             and do (set-msg-border-width 10)
             and do (message "Battery low!")
           else
             do (set-border-color "white")
             and do (set-fg-color "white")
             and do (set-msg-border-width 1)
           do (stumpwm::sync-all-frame-windows (current-group))))))

(define-key *top-map* (kbd "s-B") "battery-info-message")
;; Even less involved / more compulsive binding.
(define-key *top-map* (kbd "s-b") "battery-info-message")
