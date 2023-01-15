(in-package :stumpwm)
(require :str)
(require :cl-ppcre)

(defun get-root-device ()
  (run-shell-command-trim
   "df -h | grep -E 'dev/sda*' | grep -v '/boot'"))



(defun disk-usage ()
  (concatenate 'string
	       "disk : "
               (get-root-device)
               ))

(defun disk-usage-command ()
  (disk-usage))
