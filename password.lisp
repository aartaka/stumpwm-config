(in-package :stumpwm-user)

(defvar *master-password* nil
  "The master password for the KeePassXC file.")

(defparameter *password-entries* ()
  "Cached list of password entries.
Used to speed up the querying part of the copying commands.")

(defun get-master-password (password-file)
  "Read master password for PASSWORD-FILE from StumpWM."
  (loop for password
          = *master-password*
            then (read-one-line (current-screen) (concat password-file " password: ") :password t)
        for attempt upto 3
        until (or
               (= attempt 3)
               (when password
                 (with-input-from-string (st password)
                   (ignore-errors
                    (uiop:run-program (list "keepassxc-cli" "ls" password-file)
                                      :input st :output '(:string :stripped t))))))
        finally (return (setf *master-password* password))))

(defun get-entries (password-file)
  (or *password-entries*
      (let* ((password (get-master-password password-file))
             (entries
               (unless (or (equal password '("NIL"))
                           (equal password nil))
                 (with-input-from-string (st password)
                   (remove-if
                    (lambda (s) (uiop:string-suffix-p s "/"))
                    (uiop:split-string
                     (uiop:run-program (list "keepassxc-cli" "ls" password-file)
                                       :input st :output '(:string :stripped t))
                     :separator '(#\Newline)))))))
        (setf *password-entries* entries))))

(defun get-entry (password-file)
  "Choose the entry from PASSWORD-FILE to work (get username/password) on."
  (let ((entries (get-entries password-file)))
    (unless (or (equal entries '("NIL"))
                (equal entries nil))
      (string-trim '(#\Space)
                   (read-one-line (current-screen) "entry: "
                                  :completions entries :require-match t)))))

(defcommand copy-password () ()
  "Copy the password for the given entity in password file."
  (let* ((password-file (home "/Documents/p.kdbx"))
         (password (get-master-password password-file))
         (entry (get-entry password-file)))
    (unless (or (equal entry '("NIL"))
                (equal entry nil))
      (with-input-from-string (st password)
        (uiop:launch-program (list "keepassxc-cli" "clip" password-file entry)
                             :input st)))))

(defcommand copy-username () ()
  "Copy the username for the given entity in password file."
  (let* ((password-file (home "/Documents/p.kdbx"))
         (password (get-master-password password-file))
         (entry (get-entry password-file)))
    (unless (or (equal entry '("NIL"))
                (equal entry nil))
      (with-input-from-string (st password)
        (uiop:launch-program
         (list "keepassxc-cli" "clip" "--attribute" "username"
               password-file entry)
         :input st)))))
