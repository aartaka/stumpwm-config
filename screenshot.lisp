(in-package :stumpwm-user)

(defcommand timestamp-screenshot () ()
  "Save a screenshot (as a timestamp-named .PNG) and open Gimp to edit it."
  (let ((file-name (format nil (home "/Documents/screenshots/~X.png") (get-universal-time))))
    (ensure-directories-exist file-name)
    (screenshot:screenshot-window file-name)
    (setf (uiop:getenv "GDK_SCALE") "1")
    (uiop:launch-program (list "gimp" file-name))
    (setf (uiop:getenv "GDK_SCALE") "2")))

(define-key *top-map* (kbd "s-s") "timestamp-screenshot")
(define-key *top-map* (kbd "SunPrint_Screen") "timestamp-screenshot")
