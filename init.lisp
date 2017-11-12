(in-package :stumpwm)
(ql:quickload :str)
(ql:quickload :cl-ppcre)
(ql:quickload :parse-float)
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

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "c") "exec xfce4-terminal")

(defun read-bat (mode)
  (let ((bat-dir "/sys/class/power_supply/BAT1/"))
    (values
     (with-open-file (stream (concatenate 'string bat-dir mode))
       (read-line stream nil)))))

(defun read-bat-status ()
  (concatenate 'string
               "charge status : "
               (read-bat "status")))

(defun read-bat-capacity ()
  (concatenate
   'string
   "battery percentage : "
   (read-bat "capacity")
   "%"))

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
  (format nil
          "~{~A~^, ~}"
          (remove-if
           (lambda (x) (= 0 (length x)))
           (cl-ppcre:split "\\|"
                           (values
                            (trim-total
                             (stumpwm:run-shell-command
                              (disk-usage-command device)
                              t)
                             "|"))))))

(defun uptime-second ()
  (parse-float:parse-float
   (car (cl-ppcre:split
         "\\s"
         (with-open-file (stream "/proc/uptime")
           (read-line stream nil))))))

(defun uptime-hour () 
  (values
   (round (/
           (uptime-second) 
           3600))))

(defun uptime-minute ()
  (values
   (round
    (/ (rem (uptime-second) 3600)
       60))))

(defun date-time ()
  (str:trim (values
             (cl-ppcre:regex-replace-all
              ":\\d+\\sWIB"
              (stumpwm:run-shell-command "date" t)
              " WIB"))))

(setf *screen-mode-line-format*
      (list "" '(:eval
                 (str:trim (values
                            (cl-ppcre:regex-replace-all
                             ":\\d+\\sWIB"
                             (stumpwm:run-shell-command "date" t)
                             " WIB"))))
            " | " '(:eval (read-bat-capacity))
            ", " '(:eval (read-bat-status))
            " | " '(:eval (disk-usage "/dev/sda3"))
            " | " '(:eval (concatenate 'string
                           "up : "
                           (write-to-string (uptime-hour))
                           " hours "
                           (write-to-string (uptime-minute)) 
                           " minutes "))))

;; turn on/off the mode line for the current head only.
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))
