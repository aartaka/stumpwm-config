(in-package :stumpwm-user)

(defcommand timestamp-screenshot () ()
  "Save a screenshot and open Gimp to edit it."
  (let ((file-name (format nil (home "/Documents/screenshots/~X.png") (get-universal-time))))
    (uiop/cl:ensure-directories-exist file-name)
    (screenshot:screenshot-window file-name)
    (uiop:launch-program (list "gimp" file-name))))

(define-key *top-map* (kbd "s-s") "timestamp-screenshot")
