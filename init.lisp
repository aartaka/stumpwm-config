(in-package :stumpwm)

;; Just in case Quicklisp won't be found. Gotta also make this install QL in case it needs to.
#-quicklisp
(let ((quicklisp-init
        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(run-shell-command "emacs --daemon")

(defprogram-shortcut emacsclient
    :command "emacsclient -cn -a '' -F '((font . \"Hack-18\") (vertical-scroll-bars) (tool-bar-lines) (menu-bar-lines))'"
    :map *root-map*
    :key (kbd "C-e"))

(run-shell-command "bash -c \"export SBCL_HOME=\\\"$(dirname -- $(dirname -- $(readlink -f $(which sbcl))))/lib/sbcl/\\\"\"")

(define-key *root-map* (kbd "e") "emacsclient")

;; Important asdf-loads
(init-load-path "~/.stumpwm.d/contrib/") ; For StumpWM contribs to be easily loaded
(asdf:load-systems :cl-ppcre :dexador :clx-truetype :zpng :swank     ; dependencies
                   :ttf-fonts :screenshot :battery-portable)   ; stumpwm-contribs

;; Start Swank server.
(swank:create-server :port 4005
                     :dont-close t)

(setf *message-window-gravity* :center
      *message-window-input-gravity* :center
      *input-window-gravity* :center
      *timeout-wait* 2

      *mode-line-pad-y* 5
      *mode-line-pad-x* 10
      *window-format* "%s %25t |"
      *screen-mode-line-format* "%B | %d | %w"

      *mouse-focus-policy* :click)


(define-key *root-map* (kbd "SunPrint_Screen") "screenshot")

;; A terrible macro to define both commands and keys conveniently.
;; It was even more horrible until I heard of defprogram-shortcut!
(defmacro define-shell-keys (&rest keybindings)
  `(progn ,@(loop :for binding :in keybindings
                  :collect
                  (let ((program (first binding)) (keys (rest binding)))
                    `(progn ,@(loop :for key :in keys
                                    :collect `(defprogram-shortcut ,program
                                                :map *root-map*
                                                :key (kbd ,key))))))))

(defclass binwarp-area ()
  ((area-x :initarg :area-x :type 'bignum :accessor area-x)
   (area-y :initarg :area-y :type 'bignum :accessor area-y)
   (height :initarg :height :type 'bignum :accessor height)
   (width  :initarg :width  :type 'bignum :accessor width)
   (pointer-x               :type 'bignum :accessor ptr-x)
   (pointer-y               :type 'bignum :accessor ptr-y)))

;; (defcommand init-binwarp () ()
;;   (let ((win (current-window)))
;;     (setf *binwarp-mode* t
;;           *binwarp-area*
;;           (make-instance binwarp-area
;;                          :area-x (window-x win)
;;                          :area-y (window-y win)
;;                          :height (window-height win)
;;                          :width  (window-width win))))
;;   (when *reinitiate-pointer*
;;     (centered-warp *binwarp-area*)))

(defvar *binwarp-mode* nil
  "Whether you are in the binwarp mode")
(defvar *binwarp-area* '(:top nil :bottom nil :right nil :left nil)
  "The current screen area that binwarp is restricted by")
(defvar *binwarp-history* nil
  "The previous binwarp areas")
(defvar *reinitiate-pointer* nil
  "Set this to non-nil if you want to start binwarping from specific point on the screen")
(defvar *clear-history* nil
  "Whether the history of binwarps cleans up after every binwarping session")
(defvar *preserve-pointer* nil
  "Whether you need the pointer position change only in the axis you are providing")

(defcommand init-binwarp () ()
  (let ((win (current-window)))
    (setf *binwarp-mode* t
          (getf *binwarp-area* :top) (window-y win)
          (getf *binwarp-area* :bottom) (+ (window-y win) (window-height win))
          (getf *binwarp-area* :left) (window-x win)
          (getf *binwarp-area* :right) (+ (window-x win) (window-width win))))
  (when *reinitiate-pointer*
    (centered-warp *binwarp-area*)))

(defcommand exit-binwarp () ()
  (setf *binwarp-mode* nil
        *binwarp-history*
        (when (not *clear-history*)
          *binwarp-history*))
  (refresh))

(defun invert-gravity (gravity)
  (case gravity
    (:left :right)
    (:right :left)
    (:top :bottom)
    (:bottom :top)))

(defun avg (&rest list)
  (/ (reduce #'+ list)
     (length list)))

(defun centered-warp (area)
  (macrolet ((ar (grav) `(getf area ,grav)))
    (ratwarp (round (avg (ar :left) (ar :right)))
             (round (avg (ar :top) (ar :bottom))))))

(defmacro with-pointer ((x-var y-var) &body body)
  `(multiple-value-bind (,x-var ,y-var)
       (xlib:global-pointer-position *display*)
     ,@body))

(defcommand back-binwarp () ()
  (when *binwarp-history*
    (setf *binwarp-area* (pop *binwarp-history*))
    (centered-warp *binwarp-area*)))

(defun vertical-p (gravity)
  (member gravity '(:top :bottom)))

(defun horisontal-p (gravity)
  (member gravity '(:right :left)))

(defcommand binwarp (gravity) (:gravity)
  "A command that splits the current *BINWARP-AREA* in two over the pointer position
and moves the pointer to the center of this area -- in the direction of the GRAVITY."
  (push *binwarp-area* *binwarp-history*)
  (with-pointer (pointer-x pointer-y)
    (setf (getf *binwarp-area* (invert-gravity gravity))
          (if (horisontal-p gravity) pointer-x pointer-y))
    (centered-warp *binwarp-area*)))

(define-interactive-keymap (binwarp-keymap tile-group)
    (:on-enter #'init-binwarp
     :on-exit #'exit-binwarp
     :exit-on ((kbd "C-g")
               (kbd "ESC")))
  ((kbd "n") "binwarp bottom")
  ((kbd "p") "binwarp top")
  ((kbd "f") "binwarp right")
  ((kbd "b") "binwarp left")

  ((kbd "C-n") "ratrelwarp  0 +5")
  ((kbd "C-p") "ratrelwarp  0 -5")
  ((kbd "C-f") "ratrelwarp +5  0")
  ((kbd "C-b") "ratrelwarp -5  0")

  ((kbd "RET") "ratclick 1")
  ((kbd "SPC") "ratclick 3")

  ((kbd "0") "init-binwarp")
  ((kbd "TAB") "back-binwarp"))

(define-key *root-map* (kbd "m") "binwarp-keymap")

(defprogram-shortcut dev
  :command "guix environment -m ~/dev-manifest.scm -- emacs"
  :pullp t
  :key (kbd "C-F8"))

(defprogram-shortcut next-dev
  :command "guix environment --load=/home/aartaka/git-cloned/nyxt/build-scripts/guix.scm nss-certs glib-networking -- emacs"
  :pullp t
  :key (kbd "C-F7"))

(defprogram-shortcut gimp
  :command "guix environment --ad-hoc gimp -- gimp"
  :pullp t
  :key (kbd "C-F6"))

(defvar *dict-uri* "http://wordnetweb.princeton.edu/perl/webwn?s=")
(defun cleanup-definition (string)
  (ppcre:regex-replace-all
   "(<[^>]*>|S:|\\([^ )]\\))"
   (ppcre:regex-replace-all
    "<h3[^>]*>"
    (ppcre:regex-replace-all
     "</h3[^>]*>"
     string
     "^r")
    "^R")
   ""))

(defcommand definition (word) ((:string "Definition of: "))
            (let ((*timeout-wait* 20))
              (message "~{~a~%~}"
                       (mapcar #'cleanup-definition
                               (ppcre:all-matches-as-strings
                                "(<h3[^>]*>.*</h3.*>|<li[^>]*>.*</li.*>)"
                                (dex:get (concatenate
                                          'string
                                          "http://wordnetweb.princeton.edu/perl/webwn?s="
                                          word)))))))

(define-key *root-map* (kbd "d") "definition")

(define-shell-keys
    (icecat "f" "C-f")
    (urxvt "c" "C-c")
  (keepassxc "p" "C-p")
  (libreoffice "l" "C-l"))

;; This section is dependent on Vimium-FF
;; That's a big lose on the side of Emacs user base
;; ... or myself not being able to fing Emacs-inspired addon for FF
(define-remapped-keys
    `((,(lambda (win)
          (and (member (window-class win)
                       '("Firefox" "IceCat" "Nightly")
                       :test #'string-equal)
               (not *binwarp-mode*)))
        ;; The native ones
        ("C-n" . "Down")
        ("C-p" . "Up")
        ("C-b" . "Left")
        ("C-f" . "Right")
        ("M-<" . "Home")
        ("M->" . "End")
        ;; Less-obvious native ones
        ("M-." . "M-Right")  ; Forward in history
        ("M-," . "M-Left")   ; Back in history
        ("C-s" . "C-f")      ; Forward search
        ("C-r" . "C-G")      ; Reverse search
        ("C-g" . "ESC")      ; Cancel whatever
        ("C-d" . "C-k")      ; Focus search bar
        ("C-o" . "C-t")      ; New tab
        ("C-k" . "C-w")      ; Close current tab
        ("C-1" . "F11")      ; Fullscreen
        ;; The ones that I owe to Vimium
        ("C-?" . "?")        ; Vimium help screen
        ("M-l" . "F")        ; Open the link in the new tab
        ("C-l" . "f")        ; Open the link in this tab
        ;; Text-editing ones
        ("C-y" . "C-v")      ; Paste/yank
        ("C-w" . "C-x")      ; Cut/kill
        ("M-w" . "C-c")      ; Copy
        ("M-f" . "C-Right")  ; Jump one word forward
        ("M-b" . "C-Left")   ; Jump one word backward
        ("C-a" . "C-Up")     ; Jump to the beginning of the line
        ("C-e" . "C-Down")   ; Jump to the end of the line
        ("C-M-a" . "C-Home") ; Jump to the beginning of the text
        ("C-M-e" . "C-End")  ; Jump to the end of the text
        ("C-SPC" . "F7"))))  ; Caret mode. Can be handy for text selection.

;; The command to set urxvt appearance up.
;; I'd better have something lispy, 'cause .Xresources confuses me.
(run-shell-command "xrdb ~/.Xresources")

;; Recommended by Guix Cookbook. Will revisit and test later.
;; (require :ttf-fonts)
;; (setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
;; (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
;; (xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11))

(enable-mode-line (current-screen) (current-head) :visible)
