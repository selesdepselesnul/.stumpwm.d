(in-package :stumpwm)
(require :str)
(require :cl-ppcre)


(setf +day-names+
      '("Monday" "Tuesday" "Wednesday"
        "Thursday" "Friday" "Saturday"
        "Sunday"))

(defun date-time () 
  (multiple-value-bind
        (_ minute hour day month year day-of-week)
      (get-decoded-time)
    (format nil
            "~a ~a:~a, ~a-~a-~a"
            (nth day-of-week +day-names+)
            hour
            minute
            day
            month
            year)))

