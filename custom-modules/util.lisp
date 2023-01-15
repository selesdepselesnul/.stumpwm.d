
(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
(require :swank)


(defun run-shell-command-trim (command)
  (str:trim
   (run-shell-command
    command
    t)))

(defun trim-total (str &optional (replacer ""))
  (cl-ppcre:regex-replace-all "\\s"
                              str
                              replacer))
