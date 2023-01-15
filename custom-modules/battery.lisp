(in-package :stumpwm)
(require :str)
(require :cl-ppcre)

(defun detect-battery-path ()
  (concatenate
   'string
   "/sys/class/power_supply/"
   (run-shell-command-trim
    "ls /sys/class/power_supply/ | grep -E 'BAT'")))


(defparameter *battery-file* (detect-battery-path))

(defun read-bat-info (label type suffix)
  (concatenate 'string
               label
	       (first (uiop:read-file-lines
                       (concatenate 'string *battery-file* "/" type)))
	       suffix))


(defun read-bat-capacity ()
  (read-bat-info "battery capacity : " "capacity" "%"))

(defun read-bat-status ()
  (read-bat-info "battery status : " "status" ""))
