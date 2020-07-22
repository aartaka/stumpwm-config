(in-package :stumpwm)

;; Just in case Quicklisp won't be found. Gotta also make this install QL in case it needs to.
#-quicklisp
(let ((quicklisp-init
        (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(run-shell-command "emacs --daemon")

(defprogram-shortcut emacsclient
    :command "emacsclient -cn -a '' -F '((font . \"Hack-17\") (vertical-scroll-bars) (tool-bar-lines) (menu-bar-lines))'"
    :map *root-map*
    :key (kbd "e"))

(define-key *root-map* (kbd "e") "emacsclient")

;; Important asdf-loads
(init-load-path "~/git-cloned/stumpwm-contrib/")
(asdf:load-systems :cl-ppcre :dexador :clx-truetype :zpng :alexandria :slynk    ; dependencies
                   :ttf-fonts :screenshot :battery-portable :binwarp)           ; stumpwm-contribs

(defvar *dict-url* "http://wordnetweb.princeton.edu/perl/webwn?s="
  "The URL for the english dict lookups")
(defvar *slynk-port* 4012 "The port to start Slynk at. Change in case of collisions.")

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

(defmacro define-shell-keys (&rest keybindings)
  "A terrible macro to define both commands and keys conveniently.
It was even more horrible until I heard of defprogram-shortcut!"
  `(progn ,@(loop :for binding :in keybindings
                  :collect
                  (let ((program (first binding)) (keys (rest binding)))
                    `(progn ,@(loop :for key :in keys
                                    :collect `(defprogram-shortcut ,program
                                                :map *root-map*
                                                :key (kbd ,key))))))))
(defcommand timestamp-screenshot () () 
 (screenshot:screenshot-window (format nil "~X.png" (get-universal-time))))

(binwarp:define-binwarp-mode binwarp-mode
    "m" (:map *root-map*)
  ((kbd "C-n") "ratrelwarp  0 +5")
  ((kbd "C-p") "ratrelwarp  0 -5")
  ((kbd "C-f") "ratrelwarp +5  0")
  ((kbd "C-b") "ratrelwarp -5  0")

  ((kbd "RET") "ratclick 1")
  ((kbd "SPC") "ratclick 3"))

(defprogram-shortcut dev
  :command "guix environment -m ~/dev-manifest.scm -- emacs"
  :pullp t
  :key (kbd "C-F8"))

(defprogram-shortcut nyxt-dev
  :command #.(concatenate 'string
                          "guix environment "
                          "-l ~/git-cloned/nyxt/build-scripts/guix.scm "
                          "glib-networking --ad-hoc nss-certs glib-networking "
                          "-- emacs")
  :pullp t
  :key (kbd "C-F7"))

(defprogram-shortcut gimp
  :command "guix environment --ad-hoc gimp -- gimp"
  :pullp t
  :key (kbd "C-F6"))

(defprogram-shortcut cl-webkit-dev
  :command "guix environment --ad-hoc glib glib-networking gdk-pixbuf cairo pango gtk+ webkitgtk -- emacs"
  :pullp t
  :key (kbd "C-F5"))

(defcommand definition (word) ((:string "Definition of: "))
  (flet ((cleanup-definition (string)
           (reduce #'(lambda (string replacements)
                       (ppcre:regex-replace-all (first replacements)
                                                string
                                                (second replacements)))
                   '(("</h3[^>]*>" "^r")
                     ("<h3[^>]*>" "^R")
                     ("(<[^>]*>|S:|\\([^ )]\\))" ""))
                   :initial-value string)))
    (let ((*timeout-wait* 20))
      (message "~{~a~%~}"
               (mapcar #'cleanup-definition
                       (ppcre:all-matches-as-strings
                        "(<h3[^>]*>.*</h3.*>|<li[^>]*>.*</li.*>)"
                        (dex:get (concatenate 'string *dict-url* word))))))))

(define-key *root-map* (kbd "d") "definition")

(define-shell-keys
    (icecat "f" "C-f")
    (urxvt "c" "C-c")
  (keepassxc "p" "C-p")
  (libreoffice "l" "C-l"))

;; This section is dependent on Vimium-FF
(define-remapped-keys
    `((,(lambda (win)
          (and (member (window-class win)
                       '("Firefox" "IceCat" "Nightly")
                       :test #'string-equal)
               (not binwarp:*binwarp-mode-p*)))
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
        ("C-w" . "ESC"))))

;; Recommended by Guix Cookbook. Will revisit and test later.
;; (require :ttf-fonts)
;; (setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
;; (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
;; (xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11))

(enable-mode-line (current-screen) (current-head) :visible)
