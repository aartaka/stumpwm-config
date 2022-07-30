(in-package :stumpwm-user)

(define-key *top-map* (kbd "s-m") "binwarp-mode")

(binwarp:define-binwarp-mode binwarp-mode
    "m" (:map *root-map* :redefine-bindings t)
  ((kbd "n") "binwarp down")
  ((kbd "p") "binwarp up")
  ((kbd "f") "binwarp right")
  ((kbd "b") "binwarp left")

  ((kbd "C-n") "binwarp down")
  ((kbd "C-p") "binwarp up")
  ((kbd "C-f") "binwarp right")
  ((kbd "C-b") "binwarp left")

  ((kbd "0") "init-binwarp")
  ((kbd "TAB") "back-binwarp")

  ((kbd "C-M-n") "ratrelwarp  0 +5")
  ((kbd "C-M-p") "ratrelwarp  0 -5")
  ((kbd "C-M-f") "ratrelwarp +5  0")
  ((kbd "C-M-b") "ratrelwarp -5  0")

  ((kbd "RET") "ratclick 1")
  ((kbd "SPC") "ratclick 3"))
