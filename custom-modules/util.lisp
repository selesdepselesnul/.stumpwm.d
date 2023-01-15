
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

(defmacro make-custom-key (command-name command-exp map key)
  `(progn
     (if (functionp ,command-exp)
         (defcommand ,command-name () () (funcall ,command-exp))
         (defcommand ,command-name () ()
           (run-or-raise ,command-exp '(:class ,(string command-name))))) 
     (define-key ,map (kbd ,key) ,(string command-name))))
