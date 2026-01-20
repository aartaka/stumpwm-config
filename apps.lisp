(in-package :stumpwm-user)

(defcommand doom () ()
  (uiop:launch-program (list "chocolate-doom" "-iwad" (home "/Documents/doom/GAME/DOOMU.WAD"))))
(defcommand doom2 () ()
  (uiop:launch-program (list "chocolate-doom" "-iwad" (home "/Documents/doom2/DOOM2.WAD"))))
(defcommand doom-plutonia () ()
  (uiop:launch-program (list "chocolate-doom" "-iwad" (home "/Documents/doom-plutonia/PLUTONIA.WAD"))))
(defcommand quake () ()
  (uiop:launch-program (list "ioquake3.x86_64" "-height" "1200" "-width" "1600" "-condebug" "-mode" "4")))

(defcommand gimp (&optional file-name) ((:string "File to open"))
  (setf (uiop:getenv "GDK_SCALE") "1")
  (uiop:launch-program (list "gimp" file-name))
  (setf (uiop:getenv "GDK_SCALE") "2"))

(defcommand inkscape () ()
  (setf (uiop:getenv "GDK_SCALE") "1")
  (uiop:launch-program (list "inkscape"))
  (setf (uiop:getenv "GDK_SCALE") "2"))

(defcommand surf (&optional args/url) ((:string "URL "))
  "Meta-command used in other commands to open URLs and search data."
  (uiop:launch-program (cons "surf" (uiop:ensure-list args/url))))
(defcommand surf-proxy () ()
  "Tor-proxied version of surf."
  (uiop:launch-program "http_proxy=\"socks5://localhost:9050\" surf"))

(defcommand rlwrap (&optional impl) ((:string "Lisp implementation "))
  "Run a chosen Common Lisp in ST with Readline (rlwrap)."
  (uiop:launch-program (list "st" "rlwrap" "-c" "-b" "(){}[],^%$#@\"\";''|\\" "-q" "\"" impl)))

(defcommand ardshin () ()
  (surf "https://ibanking.ardshinbank.am:4443/nibanking/login"))
(defcommand disroot () ()
  (surf '("-S" "https://webmail.disroot.org")))
(defcommand proton () ()
  (surf '("-S" "https://proton.me")))
(defcommand youtube () ()
  (surf '("-SI" "https://yewtu.be/")))
(defcommand merveilles () ()
  (surf '("-SI" "https://merveilles.town")))
(defcommand dict (&optional word) ((:string "Word to search: "))
  (surf (format nil "http://wordnetweb.princeton.edu/perl/webwn?s=~a" word)))
(defcommand news () ()
  (surf "https://news.ycombinator.com"))

(defcommand query (query) ((:string "Search query: "))
  "Search the text via Surf."
  (unless (member query '("" "NIL" nil) :test #'equalp)
    (uiop:launch-program (format nil "surf \" ~a\"" query))))
(defcommand copy-surf-link () ()
  "A hack until I figure out how to copy links directly from Surf."
  (trivial-clipboard:text
   (second
    (uiop:split-string
     (alexandria:lastcar
      (uiop:read-file-lines #p"~/.local/share/surf/history.txt"))))))

(defcommand discord () ()
  (uiop:launch-program (list "flatpak" "run" "com.discordapp.Discord")))
