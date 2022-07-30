(in-package :stumpwm-user)

(setf mem::*mem-usage-bar-empty* #\Space
      mem::*mem-modeline-fmt* "^07[^70 RAM%p^07]^70"
      *screen-mode-line-format* (concat "%M " *screen-mode-line-format*))
