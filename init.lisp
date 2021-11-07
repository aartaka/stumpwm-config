(in-package :stumpwm)

;; Just in case Quicklisp won't be found. Gotta also make this install QL in case it needs to.
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Important asdf-loads
(init-load-path "~/git/stumpwm-contrib/")
(ql:quickload
 '(:cl-ppcre :dexador :clx-truetype :zpng :alexandria :slynk :bordeaux-threads)) ; dependencies
(asdf:load-systems :ttf-fonts :screenshot :battery-portable :binwarp)            ; stumpwm-contribs

(uiop:launch-program '("emacs" "--daemon") :directory (user-homedir-pathname))

(defvar *slynk-port* 4012 "The port to start Slynk at. Change in case of collisions.")

(defvar *battery-thread*
  (bt:make-thread
   #'(lambda ()
       (let ((*message-window-gravity* :top-left))
         (loop :for time := (get-universal-time)
               :when (= 0 (mod time 40))
                 :do (battery-info-message))))
   :name "Battery reporting")
  "A small thread for battery state reporting. Made to get rid of modeline.")

(setf *message-window-gravity* :center
      *message-window-input-gravity* :center
      *input-window-gravity* :center
      *timeout-wait* 2

      *startup-message* "Happy window hacking!"

      *mode-line-pad-y* 5
      *mode-line-pad-x* 10
      *window-format* "%s %25t |"
      *screen-mode-line-format* "%B | %d | %w"

      *mouse-focus-policy* :click)

;; Command to start Slynk server.
(defcommand start-slynk () ()
  (slynk:create-server :port *slynk-port*
                       :dont-close t))

(defcommand timestamp-screenshot () ()
  (screenshot:screenshot-window (format nil "~X.png" (get-universal-time))))

(defcommand battery-info-message () ()
  (message (battery-portable::battery-info-string)))

(binwarp:define-binwarp-mode binwarp-mode
    "m" (:map *root-map* :redefine-bindings t)
  ((kbd "n") "binwarp down")
  ((kbd "p") "binwarp up")
  ((kbd "f") "binwarp right")
  ((kbd "b") "binwarp left")

  ((kbd "0") "init-binwarp")
  ((kbd "TAB") "back-binwarp")

  ((kbd "C-M-n") "ratrelwarp  0 +5")
  ((kbd "C-M-p") "ratrelwarp  0 -5")
  ((kbd "C-M-f") "ratrelwarp +5  0")
  ((kbd "C-M-b") "ratrelwarp -5  0")

  ((kbd "RET") "ratclick 1")
  ((kbd "SPC") "ratclick 3"))

(set-prefix-key (kbd "s-t"))

(dolist
    (binding `((,(kbd "s-r")   "iresize")
               (,(kbd "s-a")   "time")
               (,(kbd "s-B")   "battery-info-message")
               (,(kbd "s-b")   "banish")
               (,(kbd "s-:")   "eval")
               (,(kbd "s-;")   "colon")
               (,(kbd "s-!")   "exec")
               (,(kbd "s-h")   ,*help-map*)
               (,(kbd "s-g")   ,*groups-map*)
               (,(kbd "s-x")   ,*exchange-window-map*)
               (,(kbd "s-q")   "send-raw-key")
               (,(kbd "s-d")   "definition")
               (,(kbd "s-m")   "binwarp-mode")
               (,(kbd "s-s")   "timestamp-screenshot")
               (,(kbd "s-e")   ,(concat "exec emacsclient -cn -a '' "
                                        "-F '((font . \"Hack-17\") (vertical-scroll-bars) "
                                        "(tool-bar-lines) (menu-bar-lines))'"))
               (,(kbd "s-C-e") "exec emacs")
               (,(kbd "s-C-t") "exec st")
               (,(kbd "s-C-n") "exec nyxt")
               (,(kbd "s-C-f") "exec firefox")
               (,(kbd "s-C-i") "exec icecat")
               (,(kbd "s-C-u") "exec urxvt")
               (,(kbd "s-C-k") "exec keepassxc")
               (,(kbd "s-C-l") "exec libreoffice")
               (,(kbd "s-C-g") "exec gimp")
               (,(kbd "<XF86AudioRaiseVolume>") . "exec pamixer --allow-boost -i 5")
               (,(kbd "<XF86AudioLowerVolume>") . "exec pamixer -d 5")
               (,(kbd "<XF86AudioMute>") . "exec bash ~/.config/emacs/mute.sh")
               (,(kbd "s-SPC") "pull-hidden-next")
               (,(kbd "s-M-p") "prev-in-frame")
               (,(kbd "s-M-n") "next-in-frame")
               (,(kbd "s-4")   "fullscreen")
               (,(kbd "s-3")   "hsplit")
               (,(kbd "s-2")   "vsplit")
               (,(kbd "s-1")   "only")
               (,(kbd "s-0")   "remove")
               (,(kbd "s-TAB") "fother")
               (,(kbd "s-w")   "windows")
               (,(kbd "s-l")   "redisplay")
               (,(kbd "s-+")   "balance-frames")
               (,(kbd "s-k")   "delete")
               (,(kbd "s-K")   "kill")
               (,(kbd "s-o")   "other")
               (,(kbd "s-i")   "info")
               (,(kbd "s-I")   "show-window-properties")
               (,(kbd "s-#")   "mark")
               (,(kbd "s-N")   "number")))
  (apply (alexandria:curry #'define-key *top-map*) binding))

;; This section is dependent on Vimium-FF
(define-remapped-keys
    `((,(lambda (win)
          (and (member (window-class win)
                       '("Firefox" "IceCat" "Nightly")
                       :test #'string-equal)
               (not binwarp:*binwarp-mode-p*)))
        ;; Less-obvious native ones
        ("M-." . "M-Right")  ; Forward in history
        ("M-," . "M-Left")   ; Back in history
        ("C-s" . "C-f")      ; Forward search
        ("C-g" . "ESC")      ; Cancel whatever
        ("C-d" . "C-k")      ; Focus search bar
        ("C-o" . "C-t")      ; New tab
        ("C-k" . "C-w")      ; Close current tab
        ("C-1" . "F11")      ; Fullscreen
        ("C-r" . "C-M-R")    ; Reader-mode?
        ("C-R" . "C-M-r")    ; Reload the page
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
        ("C-SPC" . "F7")     ; Caret mode. Can be handy for text selection.
        ;; Unbind the quirky original keybindings
        ("C-t" . "ESC")
        ("C-q" . "ESC"))))

(define-remapped-keys
    `((,(lambda (win) (declare (ignorable win)) (not binwarp:*binwarp-mode-p*))
        ;; The native ones
        ("C-n" . "Down")
        ("C-p" . "Up")
        ("C-b" . "Left")
        ("C-f" . "Right")
        ("M-<" . "Home")
        ("M->" . "End")
        ("C-a" . "Home")
        ("C-e" . "End")
        ("C-d" . "Delete"))))

;; Recommended by Guix Cookbook. Will revisit and test later.
(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
(xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "Hack" :subfamily "Regular" :size 17))
