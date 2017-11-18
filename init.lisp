(in-package :stumpwm)
(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :parse-float)
(ql:quickload :drakma)
(ql:quickload :yason)
(require :swank)

(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
;; Web jump
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
               (substitute #\+ #\Space search)
               (run-shell-command (concatenate 'string ,prefix search))))

(defun on-default-browser (url)
  (concatenate 'string "google-chrome-stable" " " url))

(make-web-jump "google" (on-default-browser "http://www.google.com/search?q="))
(make-web-jump "youtube" (on-default-browser "https://www.youtube.com/results?search_query="))
(make-web-jump "github-trending" (on-default-browser "https://github.com/trending/"))
(make-web-jump "wiki" (on-default-browser "https://en.wikipedia.org/wiki/"))

(defcommand github-selesdepselesnul ()
    () (run-shell-command
        (on-default-browser
         "https://github.com/selesdepselesnul")))

(defun read-bat (mode)
  (let ((bat-dir "/sys/class/power_supply/BAT1/"))
    (string-downcase
     (values
      (with-open-file (stream (concatenate 'string bat-dir mode))
        (read-line stream nil))))))

(defun read-bat-capacity ()
  (concatenate
   'string
   "battery : "
   (read-bat "capacity")
   "%"))

(defun read-bat-status ()
  (concatenate 'string
               "status : "
               (read-bat "status")))

(defun trim-total (str &optional (replacer ""))
  (cl-ppcre:regex-replace-all "\\s"
                              str
                              replacer))

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
                 (nth 2 disk)
                 ", used % : "
                 (nth 4 disk))))

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

(defconstant *day-names*
  '("Monday" "Tuesday" "Wednesday"
    "Thursday" "Friday" "Saturday"
    "Sunday"))

(defun tz->str (tz)
  (let ((tz-str (write-to-string (- tz))))
    (concatenate 'string "GMT "
                 (if (< tz 0)
                     (concatenate 'string "+" tz-str)
                     (concatenate 'string "-" tz-str)))))

(defun date-time () 
  (multiple-value-bind
        (_ minute hour day month year day-of-week __ tz)
      (get-decoded-time)
    (format nil
            "~a ~a:~a ~a, ~a-~a-~a"
            (nth day-of-week *day-names*)
            hour
            minute
            (tz->str tz)
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
            " | " '(:eval (read-bat-capacity))
            ", " '(:eval (read-bat-status))
            " | " '(:eval (disk-usage "/dev/sda3"))
            " | " '(:eval (concatenate 'string
                           "up : "
                           (let ((uptime (uptime-hour)))
                             (when (> uptime 0)
                               (concatenate 'string (write-to-string uptime) " hours ")))
                           (write-to-string (uptime-minute)) 
                           " minutes "))
            " | " '(:eval (check-connection))))

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

(defcommand programming-quote-command () () (programming-quote)) 

(defcommand termite-command () ()
  "run termite"
  (run-or-raise "termite" '(:class "termite-command")))

(defcommand htop-command () () 
  (run-or-raise "termite -e htop" '(:class "termite-command")))

(defcommand wifi-menu-command () ()
  (run-shell-command "termite -e 'sudo wifi-menu'"))

(defcommand vnstat-command () ()
  (run-or-raise "termite -e 'vnstat -l'" '(:class "vnstat-command")))

(defcommand tor-browser-command () ()
  (run-or-raise "tor-browser" '(:class "tor-browser-command")))

(defcommand vlc-command () ()
  (run-or-raise "vlc" '(:class "vlc-command")))

(defcommand google-chrome-command () ()
  (run-or-raise "google-chrome-stable" '(:class "google-chrome-command")))

(defcommand okular-command () ()
  (run-or-raise "okular" '(:class "okular-command")))

(defcommand spotify-command () ()
  (run-or-raise "spotify" '(:class "spotify-command")))

(define-key *root-map* (kbd "q") "programming-quote-command")
(define-key *root-map* (kbd "c") "termite-command")
(define-key *top-map* (kbd "s-p") "htop-command")
(define-key *top-map* (kbd "s-w") "wifi-menu-command")
(define-key *top-map* (kbd "s-n") "vnstat-command")
(define-key *top-map* (kbd "s-t") "tor-browser-command")
(define-key *top-map* (kbd "s-v") "vlc-command")
(define-key *top-map* (kbd "s-g") "google-chrome-command")
(define-key *top-map* (kbd "s-r") "okular-command")
(define-key *top-map* (kbd "s-m") "spotify-command")

;; turn on/off the mode line for the current head only.
(toggle-mode-line (current-screen) (current-head))

