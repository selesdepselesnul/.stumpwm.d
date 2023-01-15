(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
(require :swank)

(setf *swank-port* 4005)

(defparameter
    *is-swank-port-not-open*
  (= (length (str:trim (run-shell-command
                        (concatenate 'string "lsof -i -P -n | grep LISTEN | grep "
                                     (write-to-string *swank-port*)) t))) 0))

(when *is-swank-port-not-open*
  (swank:create-server :PORT *swank-port*))

