(in-package :stumpwm-user)

(defcommand doom () ()
 (uiop:launch-program (list "chocolate-doom" "-iwad" (home "/Documents/doom/GAME/DOOMU.WAD"))))
(defcommand doom2 () ()
  (uiop:launch-program (list "chocolate-doom" "-iwad" (home "/Documents/doom2/DOOM2.WAD"))))
(defcommand doom-plutonia () ()
  (uiop:launch-program (list "chocolate-doom" "-iwad" (home "/Documents/doom-plutonia/PLUTONIA.WAD"))))
(defcommand quake () ()
  (uiop:launch-program (list "ioquake3.x86_64" "-height" "1200" "-width" "1600" "-condebug" "-mode" "4")))

(defcommand ardshin () ()
  (uiop:launch-program (list "surf" "https://ibanking.ardshinbank.am:4443/nibanking/login")))
(defcommand disroot () ()
  (uiop:launch-program (list "surf" "https://webmail.disroot.org")))
(defcommand proton () ()
  (uiop:launch-program (list "surf" "-S" "https://proton.me")))
(defcommand youtube () ()
  (uiop:launch-program (list "surf" "-S" "https://invidious.slipfox.xyz/search")))
