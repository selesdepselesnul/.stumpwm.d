(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
(require :parse-float)
(require :drakma)
(require :yason)
(require :swank)

(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

(setq *startup-message* "Welcome, are you ready to code ?")

(add-hook *start-hook* #'emacs)

(defun trim-total (str &optional (replacer ""))
  (cl-ppcre:regex-replace-all "\\s"
                              str
                              replacer))

(defun read-bat ()
  (str:trim (stumpwm:run-shell-command
             "batu-lepie --all"
             t)))


(defun disk-usage-command (device)
  (str:trim
   (concatenate 'string
                "df -h | grep -E "
                device)))

(defun disk-usage (device)
  (let ((disk (remove-if
               (lambda (x) (= 0 (length x)))
               (cl-ppcre:split "\\|"
                               (values
                                (trim-total
                                 (stumpwm:run-shell-command
                                  (disk-usage-command device)
                                  t)
                                 "|"))))))
    (concatenate 'string
                 "total : "
                 (nth 1 disk)
                 ", used : "
                 (nth 2 disk))))

(defun uptime-second ()
  (parse-float:parse-float
   (car (cl-ppcre:split
         "\\s"
         (with-open-file (stream "/proc/uptime")
           (read-line stream nil))))))

(defun uptime-hour () 
  (values
   (truncate
    (uptime-second) 
    3600)))


(defun uptime-minute ()
  (values
   (round
    (/ (rem (uptime-second) 3600)
       60))))

(defconstant +day-names+
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

(defun check-connection ()
  (values
   (trim-total (stumpwm:run-shell-command
                (concatenate 'string
                             "ping -q -w 1 -c 1 `ip r "
                             "| grep default"   
                             "| cut -d ' ' -f 3` > /dev/null && echo connected"
                             "|| echo disconnected")
                t))))

(defun check-brigthness ()
  (concatenate 'string
               "brigthness : "
               (trim-total (stumpwm:run-shell-command "caang" t))))

(setf *mode-line-background-color*
      "white")

(setf *mode-line-foreground-color*
      "black")

(setf *message-window-gravity*
      :center)

(setf *input-window-gravity*
      :center)

(setf *screen-mode-line-format*
      (list "" '(:eval (date-time))
            " | " '(:eval (read-bat))
            " | " '(:eval (disk-usage "/dev/sda3"))
            " | " '(:eval (concatenate 'string
                           "up : "
                           (let ((uptime (uptime-hour)))
                             (when (> uptime 0)
                               (concatenate 'string (write-to-string uptime) " hours ")))
                           (write-to-string (uptime-minute)) 
                           " minutes "))
            " | " '(:eval (check-connection))
            " | " '(:eval (check-brigthness))))

(defun newline-if-max (str max-length)
  (labels ((func (str1 str2)
             (if (> (length str2) max-length)
                 (func
                  (concatenate 'string str1 (subseq str2 0 max-length) "~%")
                  (subseq str2 max-length))
                 (concatenate 'string str1 str2))))
    (func "" str)))

(defun programming-quote ()
  (let ((stream (drakma:http-request "http://quotes.stormconsultancy.co.uk/random.json"
                                     :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let* ((result (yason:parse stream :object-as :plist))
           (author (nth 1 result))
           (quotes (nth 5 result)))
      (message
       (concatenate 'string
                    (newline-if-max quotes 140)
                    "~%"
                    author)))))

(defmacro make-custom-key (command-name command-exp map key)
  `(progn
     (if (functionp ,command-exp)
         (defcommand ,command-name () () (funcall ,command-exp))
         (defcommand ,command-name () ()
           (run-or-raise ,command-exp '(:class ,(string command-name))))) 
     (define-key ,map (kbd ,key) ,(string command-name))))

(defcommand caang-command (caang)
    ((:string "Enter brigthness: "))
  (run-or-raise (concatenate 'string "termite -e 'sudo -S caang " caang "'")
                '(:class "brigthness-command")))

(defun ask-sudo-password ()
  (read-one-line (current-screen) "fill the password : "))

(defun adjust-caang (caang)
  (if (probe-file
       (make-pathname :directory
                      '(:absolute "")
                      :name
                      "/tmp/stumpwm_password"))
      (str:trim (stumpwm:run-shell-command
                 (concatenate 'string
                              "sudo -S caang "
                              caang
                              " < /tmp/stumpwm_password")
                 t))
      (stumpwm:run-shell-command
       (concatenate 'string
                    "echo "
                    (ask-sudo-password)
                    " > /tmp/stumpwm_password")
       t)))

(defcommand caang-add-command () ()
  (adjust-caang "+1"))

(defcommand caang-sub-command () ()
  (adjust-caang "-1"))

;; custom-key
(define-key *top-map* (kbd "s-F5") "refresh")
(define-key *top-map* (kbd "s-+") "caang-add-command")
(define-key *top-map* (kbd "s--") "caang-sub-command")
(define-key *top-map* (kbd "s-=") "caang-command")
(make-custom-key termite "termite" *root-map* "c")
(make-custom-key programming-quote-command #'programming-quote *root-map* "q")
(make-custom-key alsa-mixer "termite -e alsamixer" *top-map* "s-a")
(make-custom-key htop "termite -e htop" *top-map* "s-p")
(make-custom-key wifi-menu "termite -e 'sudo wifi-menu'" *top-map* "s-w")
(make-custom-key vnstat "termite -e 'vnstat -l'" *top-map* "s-n")
(make-custom-key tor-browser "tor-browser" *top-map* "s-t")
(make-custom-key vlc "vlc" *top-map* "s-v")
(make-custom-key okular "okular" *top-map* "s-r")
(make-custom-key spotify "spotify" *top-map* "s-m")
(make-custom-key poweroff "termite -e 'sudo systemctl poweroff'" *top-map* "s-k")
(make-custom-key reboot "termite -e 'sudo systemctl reboot'" *top-map* "s-b")
(make-custom-key postman "postman" *top-map* "s-u")
(make-custom-key google-chrome "google-chrome-stable" *top-map* "s-g")

;; turn on/off the mode line for the current head only.
(toggle-mode-line (current-screen) (current-head))



