
(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
(require :swank)


(defun run-shell-command-trim (command)
  (str:trim
   (run-shell-command
    command
    t)))