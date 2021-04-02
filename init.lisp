(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
(require :swank)

(load-module "ttf-fonts")
(set-font (make-instance 'xft:font :family "Noto Serif" :subfamily "Regular" :size 10))

(setf *swank-port* 4005)

(defparameter
  *is-swank-port-not-open*
  (= (length (str:trim (run-shell-command
           (concatenate 'string "lsof -i -P -n | grep LISTEN | grep "
                        (write-to-string *swank-port*)) t))) 0))

(when *is-swank-port-not-open*
  (swank:create-server :PORT *swank-port*))

(setq *startup-message* "Welcome, are you ready to code ?")

(defun trim-total (str &optional (replacer ""))
  (cl-ppcre:regex-replace-all "\\s"
                              str
                              replacer))

(defparameter *battery-file* "/sys/class/power_supply/BAT0")

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

(defun check-vol ()
  (str:trim
   (concatenate 'string
                "volume : "
                (run-shell-command
                 "atur-polum --current"
                 t))))

(defun disk-usage ()
  (concatenate 'string
	       "disk : "
	       (str:trim (run-shell-command "df -h | grep -E 'sda1' " t))))

(defun disk-usage-command ()
  (disk-usage))

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
            " | " '(:eval (read-bat-capacity))
            " | " '(:eval (read-bat-status))
            " | " '(:eval (disk-usage))
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
(defun run-xinput (option device-id)
  (let ((x-input-command (concatenate
                         'string
                          "xinput "
                          "--"
                          option
                          " "
                          (write-to-string device-id))))  
       (run-shell-command x-input-command)))

(defun enable-device (is-enable device-id)
  (if is-enable
      (run-xinput "enable" device-id)
      (run-xinput "disable" device-id)))

(defun check-device-status-raw (device-id)
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

(defun check-device-status (device-id)
  (let ((raw-status (check-device-status-raw device-id)))
    (string= "1" raw-status)))

(defun is-keyboard-and-touchpad-enabled ()
  (every #'identity
         (map
          'list
          #'(lambda (x) (check-device-status x)) 
          '(16 17))))

(defun enable-keyboard-and-touchpad (enable)
  (mapcar #'(lambda (x) (enable-device enable x)) '(16 17)))

(defcommand selesdepselesnul/enable-keyboard-and-touchpad () ()
  (let ((is-keyboard-and-touchpad-enabled  (is-keyboard-and-touchpad-enabled)))
    (if is-keyboard-and-touchpad-enabled
        (progn
          (enable-keyboard-and-touchpad nil)
          (message "disable internal keyboard and touchpad"))
        (progn
          (enable-keyboard-and-touchpad t)
          (message "enable internal keyboard and touchpad")))))

 	
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
(make-custom-key selesdepselesnul/lxterminal "lxterminal" *top-map* "s-c")
(make-custom-key selesdepselesnul/htop "termite -e htop" *top-map* "s-p")
(make-custom-key selesdepselesnul/wifi-menu "termite -e 'sudo wifi-menu'" *top-map* "s-w")
(make-custom-key selesdepselesnul/vnstat "termite -e 'vnstat -l'" *top-map* "s-n")
(make-custom-key selesdepselesnul/poweroff "termite -e 'sudo systemctl poweroff'" *top-map* "s-F1")
(make-custom-key selesdepselesnul/reboot "termite -e 'sudo systemctl reboot'" *top-map* "s-F2")
(make-custom-key selesdepselesnul/tor-browser "tor-browser" *top-map* "s-t")
(make-custom-key selesdepselesnul/vlc "vlc" *top-map* "s-v")
(make-custom-key selesdepselesnul/okular "okular" *top-map* "s-r")
(make-custom-key selesdepselesnul/postman "postman" *top-map* "s-u")
(make-custom-key selesdepselesnul/brave "brave" *top-map* "s-b")
(make-custom-key selesdepselesnul/thunar "thunar" *top-map* "s-f")

;; turn on/off the mode line for the current head only.
(toggle-mode-line (current-screen) (current-head))
