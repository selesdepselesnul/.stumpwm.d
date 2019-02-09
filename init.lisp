(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
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
                 (run-shell-command
                  "batu-lepie --capacity"
                  t)
                 "%, "
                 (run-shell-command
                  "batu-lepie --status"
                  t)))
   ""))


(defun check-vol ()
  (str:trim
   (concatenate 'string
                "volume : "
                (run-shell-command
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
                               (trim-total
                                (run-shell-command
                                 (disk-usage-command device)
                                 t)
                                "|")))))
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
  (trim-total (run-shell-command
               (concatenate 'string
                            "ping -q -w 1 -c 1 `ip r "
                            "| grep default"   
                            "| cut -d ' ' -f 3` > /dev/null && echo connected"
                            "|| echo disconnected")
               t)))

(defun check-uptime ()
  (str:trim (run-shell-command
             "uptime -p"
             t)))

(setf *mode-line-background-color*
      "black")

(setf *mode-line-border-color*
      "grey")

(setf *mode-line-foreground-color*
      "white")

(setf *message-window-gravity*
      :center)

(setf *input-window-gravity*
      :center)

(setf *screen-mode-line-format*
      (list "" '(:eval (date-time))
            " | " '(:eval (read-bat))
            " | " '(:eval (disk-usage "/dev/sda3"))
            " | " '(:eval (check-uptime))))

(defmacro make-custom-key (command-name command-exp map key)
  `(progn
     (if (functionp ,command-exp)
         (defcommand ,command-name () () (funcall ,command-exp))
         (defcommand ,command-name () ()
           (run-or-raise ,command-exp '(:class ,(string command-name))))) 
     (define-key ,map (kbd ,key) ,(string command-name))))

(defun ask-sudo-password ()
  (read-one-line (current-screen)
                 "fill the password : "
                 :password
                 t))

(defun clear-sudo-password (password-path)
  (with-open-file (s password-path)
    (delete-file s)))

;;;; xinput
(defun run-xinput (option)
  (let ((x-input-command (concatenate
                         'string
                          "xinput "
                          "--"
                          option
                          " 16;"
                          "xinput "
                          "--"
                          option
                          " 17")))  
       (run-shell-command x-input-command)))

(defun enable-keyboard-and-touchpad (is-enable)
  (if is-enable
      (run-xinput "enable")
      (run-xinput "disable")))

(defun check-device-status (device-id)
  (let ((result
         (run-shell-command
          (concatenate
           'string
           "xinput --list-props "
           (write-to-string device-id) 
           " "
           " | grep -E 'Device Enabled'"
           " | cut -d : -f 2 |"
           "tr -d '\t\r\n'")
          t)))
    (string-trim 
      '(#\Space #\Newline #\Backspace #\Tab 
        #\Linefeed #\Page #\Return #\Rubout)
      result)))

;;(check-device-status 17)
;;(check-device-status 17)
;;;;xinput --list-props 17 | grep -E 'Device Enabled' | cut -d : -f 2 | tr -d " \t\r" 

(enable-keyboard-and-touchpad nil)
;;;;;;;;;;;;;;;;;;
 	

(defun do-with-sudo (func)
  (let ((password-temp-path "/tmp/stumpwm_password"))
    (unless (probe-file
             (make-pathname :directory
                            '(:absolute "")
                            :name
                            password-temp-path))
      (run-shell-command
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
  (run-shell-command
   (concatenate 'string
                "sudo -S "
                command
                " < "
                password)
   t))

(defcommand selesdepselesnul/poweroff ()
  ()
  (do-with-sudo
      #'(lambda (x) (run-sudo-shell-command
                "systemctl poweroff "
                x))))

(defcommand selesdepselesnul/reboot ()
  ()
  (do-with-sudo
      #'(lambda (x) (run-sudo-shell-command
                "systemctl reboot "
                x))))

;; windows
(defcommand selesdepselesnul/kill-windows-other-groups () ()
  "Kill all windows in all groups except the current group."
  (let ((target-groups (remove (current-group)
                               (screen-groups
                                 (current-screen)))))
    (dolist (group target-groups)
      (kill-windows-in-group group))))

(defcommand selesdepselesnul/kill-windows-other () ()
  "Kill all windows in current group except the current-window"
  (let ((target-windows (remove (current-window)
                                (group-windows (current-group)))))
    (kill-windows target-windows)))

(defcommand selesdepselesnul/kill-windows-any-group (in-group) ((:group "In Group: "))
  (kill-windows-in-group in-group))

;; group
(defun kill-group-with-windows (group)
  (let ((screen (group-screen group)))
    (setf (screen-groups screen) (remove group (screen-groups screen)))
    (netwm-update-groups screen)
    (netwm-set-group-properties screen)))

(defcommand selesdepselesnul/gkill-other-with-windows () ()
  "Kill other groups and all of its windows."
  (let ((groups (remove (current-group)
                        (screen-groups (current-screen)))))
    (dolist (dead-group groups)
      (kill-group-with-windows dead-group))))

(defcommand selesdepselesnul/gkill-with-windows () ()
  "Kill current group and all of its windows."
  (when-let* ((current-group (current-group))
              (groups (screen-groups (current-screen)))
              ;; If no "visible" group is found, try with all groups
              (next-group
                (or (next-group current-group (non-hidden-groups groups))
                    (next-group current-group groups))))
    (switch-to-group next-group)
    (kill-group-with-windows current-group)))

(defcommand selesdepselesnul/battery-info () ()
  (message (read-bat)))

;; custom-key
(define-key *top-map* (kbd "s-F5") "refresh")
(define-key *top-map* (kbd "s-k") "selesdepselesnul/poweroff")
(define-key *top-map* (kbd "s-b") "selesdepselesnul/reboot")
(define-key *top-map* (kbd "s-q") "quit")
(make-custom-key selesdepselesnul/lxterminal "lxterminal" *root-map* "c")
(make-custom-key selesdepselesnul/htop "termite -e htop" *top-map* "s-p")
(make-custom-key selesdepselesnul/wifi-menu "termite -e 'sudo wifi-menu'" *top-map* "s-w")
(make-custom-key selesdepselesnul/vnstat "termite -e 'vnstat -l'" *top-map* "s-n")
(make-custom-key selesdepselesnul/tor-browser "tor-browser" *top-map* "s-t")
(make-custom-key selesdepselesnul/vlc "vlc" *top-map* "s-v")
(make-custom-key selesdepselesnul/okular "okular" *top-map* "s-r")
(make-custom-key selesdepselesnul/spotify "spotify" *top-map* "s-m")
(make-custom-key selesdepselesnul/postman "postman" *top-map* "s-u")
(make-custom-key selesdepselesnul/google-chrome "google-chrome-stable" *top-map* "s-g")
(make-custom-key selesdepselesnul/virtualbox "virtualbox" *top-map* "s-x")
(make-custom-key selesdepselesnul/thunar "thunar" *top-map* "s-f")

;; turn on/off the mode line for the current head only.
(toggle-mode-line (current-screen) (current-head))
