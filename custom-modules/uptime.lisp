(in-package :stumpwm)
(require :str)
(require :cl-ppcre)

(defun check-uptime ()
  (str:trim (run-shell-command
             "uptime -p"
             t)))
