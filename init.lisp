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

(defun trim-total (str &optional (replacer ""))
  (cl-ppcre:regex-replace-all "\\s"
                              str
                              replacer))

(defun read-bat ()
  (cl-ppcre:regex-replace-all
   "\\n"
   (str:trim
    (concatenate 'string
                 "bat : "
                 (stumpwm:run-shell-command
                  "batu-lepie --capacity"
                  t)
                 "%, "
                 (stumpwm:run-shell-command
                  "batu-lepie --status"
                  t)))
   ""))

(defun check-vol ()
  (str:trim
   (concatenate 'string
                "vol : "
                (stumpwm:run-shell-command
                 "atur-polum --current"
                 t))))

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
  (trim-total (stumpwm:run-shell-command
               (concatenate 'string
                            "ping -q -w 1 -c 1 `ip r "
                            "| grep default"   
                            "| cut -d ' ' -f 3` > /dev/null && echo connected"
                            "|| echo disconnected")
               t)))

(defun check-uptime ()
  (str:trim (stumpwm:run-shell-command
             "uptime -p"
             t)))

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
            " | " '(:eval (check-uptime))
            " | " '(:eval (check-connection))
            " | " '(:eval (check-brigthness))
            " | " '(:eval (check-vol))))

(defmacro make-custom-key (command-name command-exp map key)
  `(progn
     (if (functionp ,command-exp)
         (defcommand ,command-name () () (funcall ,command-exp))
         (defcommand ,command-name () ()
           (run-or-raise ,command-exp '(:class ,(string command-name))))) 
     (define-key ,map (kbd ,key) ,(string command-name))))

(defun ask-sudo-password ()
  (read-one-line (current-screen) "fill the password : " :password t))

(defun clear-sudo-password (password-path)
  (with-open-file (s password-path)
    (delete-file s)))

(defun do-with-sudo (func)
  (let ((password-temp-path "/tmp/stumpwm_password"))
    (unless (probe-file
             (make-pathname :directory
                            '(:absolute "")
                            :name
                            password-temp-path))
      (stumpwm:run-shell-command
       (concatenate 'string
                    "echo "
                    (ask-sudo-password)
                    " > "
                    password-temp-path)
       t))
    (let ((ret-val (funcall func password-temp-path)))
      (if (string= "" ret-val)
          (progn
            (clear-sudo-password password-temp-path)
            (do-with-sudo func))
          (message ret-val)))))

(defun run-sudo-shell-command (command password)
  (stumpwm:run-shell-command
   (concatenate 'string
                "sudo -S "
                command
                " < "
                password)
   t))

(defun adjust-caang (caang)
  (do-with-sudo
      #'(lambda (x) (run-sudo-shell-command
                (concatenate 'string
                             "caang "
                             caang) 
                x))))

(defcommand poweroff-command ()
  ()
  (do-with-sudo
      #'(lambda (x) (run-sudo-shell-command
                "systemctl poweroff "
                x))))

(defcommand reboot-command ()
  ()
  (do-with-sudo
      #'(lambda (x) (run-sudo-shell-command
                "systemctl reboot "
                x))))

(defcommand caang-command (caang)
    ((:string "Enter brigthness: "))
  (do-with-sudo
      #'(lambda (x) (str:trim
                (stumpwm:run-shell-command
                 (concatenate 'string
                              "sudo -S caang "
                              caang
                              " < "
                              x)
                 t)))))

(defun adjust-volume (vol)
  (str:trim (stumpwm:run-shell-command
             (concatenate 'string
                          "atur-polum "
                          vol)
             t)))

(defun group-length () 
  (length
   (sort-groups
    (current-screen))))

(defun make-next-group ()
  (let ((group-count (+ 1 (group-length)) ))
    (if (<= group-count 10)
        (progn
          (gnew (write-to-string group-count))
          (message (concatenate 'string
                                "group number "
                                (write-to-string group-count) 
                                " created"))) 
        (message "group reach max number"))))

(defcommand make-next-group-command () ()
  (make-next-group))

(defcommand volume-add-command () ()
  (adjust-volume "+1"))

(defcommand volume-sub-command () ()
  (adjust-volume "-1"))

(defcommand volume-command (volume)
    ((:string "Enter volume: "))
  (adjust-volume volume))

(defcommand caang-add-command () ()
  (adjust-caang "+1"))

(defcommand caang-sub-command () ()
  (adjust-caang "-1"))

(defcommand gkill-other () ()
  "Kill other groups. All windows in other groups are migrated
to the current group."
  (let* ((current-group (current-group))
         (groups (remove-if (lambda (g) (eq g current-group))
                            (screen-groups (current-screen)))))
    (if (> (length groups) 0)
        (progn
          (dolist (dead-group groups)
            (kill-group dead-group current-group))
          (message "Deleted")) 
        (message "There's only one group left"))))

;; define-key fnew 
(defun define-gnew (i)
  (let* ((x-str (write-to-string i))
         (key (concatenate 'string "s-" x-str))
         (command (concatenate 'string "gnew " x-str)))
    (define-key *top-map* (kbd key) command)))

(defun range (start end)
  (loop for i from start below end collect i))

(dolist (x (range 1 10))
  (define-gnew x))

;; custom-key
(define-key *top-map* (kbd "s-F5") "refresh")
(define-key *top-map* (kbd "s-+") "caang-add-command")
(define-key *top-map* (kbd "s--") "caang-sub-command")
(define-key *top-map* (kbd "s-=") "caang-command")
(define-key *top-map* (kbd "s-)") "volume-add-command")
(define-key *top-map* (kbd "s-(") "volume-sub-command")
(define-key *top-map* (kbd "s-*") "volume-command")
(define-key *top-map* (kbd "s-G") "make-next-group-command")
(define-key *top-map* (kbd "s-k") "poweroff-command")
(define-key *top-map* (kbd "s-b") "reboot-command")
(define-key *top-map* (kbd "s-q") "quit")
(make-custom-key termite "termite" *root-map* "c")
(make-custom-key alsa-mixer "termite -e alsamixer" *top-map* "s-a")
(make-custom-key htop "termite -e htop" *top-map* "s-p")
(make-custom-key wifi-menu "termite -e 'sudo wifi-menu'" *top-map* "s-w")
(make-custom-key vnstat "termite -e 'vnstat -l'" *top-map* "s-n")
(make-custom-key tor-browser "tor-browser" *top-map* "s-t")
(make-custom-key vlc "vlc" *top-map* "s-v")
(make-custom-key okular "okular" *top-map* "s-r")
(make-custom-key spotify "spotify" *top-map* "s-m")
(make-custom-key postman "postman" *top-map* "s-u")
(make-custom-key google-chrome "google-chrome-stable" *top-map* "s-g")

;; turn on/off the mode line for the current head only.
(toggle-mode-line (current-screen) (current-head))
