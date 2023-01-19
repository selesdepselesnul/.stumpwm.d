(in-package :stumpwm)

(ignore-errors
 (load-module "ttf-fonts"))

(ignore-errors
 (set-font (make-instance 'xft:font :family "Noto Serif" :subfamily "Regular" :size 12)))
