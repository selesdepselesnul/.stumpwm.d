(in-package :stumpwm)
(ql:quickload :str)
(ql:quickload :cl-ppcre)
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

(defun upower (mode)
  (concatenate 'string
               "upower -i /org/freedesktop/UPower/devices/battery_BAT1| grep -E "
               "'"
               mode
               "'"))

(defvar bat (upower "percentage"))
(defvar charge-status (upower "state"))

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

(defun uptime ()
  (values
   (cl-ppcre:regex-replace-all
    ","
    (nth 4
         (cl-ppcre:split
          "\\s"
          (stumpwm:run-shell-command "uptime" t)))
    "")))

(setf *screen-mode-line-format*
      (list "" '(:eval
                 (str:trim (values
                            (cl-ppcre:regex-replace-all
                             ":\\d+\\sWIB"
                             (stumpwm:run-shell-command "date" t)
                             " WIB"))))
            " | " '(:eval
                    (values
                     (cl-ppcre:regex-replace-all
                      "percentage : "
                      (trim-total (stumpwm:run-shell-command bat t))
                      "battery-percentage : ")))
            " | " '(:eval (trim-total (stumpwm:run-shell-command charge-status t)))
            " | " '(:eval (disk-usage "/dev/sda3"))
            " | " '(:eval (uptime))))

;; turn on/off the mode line for the current head only.
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))
