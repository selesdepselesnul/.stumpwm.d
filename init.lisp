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
    () (run-shell-command (on-default-browser "https://github.com/selesdepselesnul")))

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "c") "exec xfce4-terminal")

(defvar bat "upower -i /org/freedesktop/UPower/devices/battery_BAT1 | grep -E percentage")

(setf *screen-mode-line-format*
      (list "" '(:eval (str:trim (stumpwm:run-shell-command "date" t)))
            " | " '(:eval
                    (values
                     (cl-ppcre:regex-replace-all
                      "percentage:"
                      (str:trim (stumpwm:run-shell-command bat t))
                      "battery-percentage    : ")))))

;; turn on/off the mode line for the current head only.
(stumpwm:toggle-mode-line (stumpwm:current-screen)
                          (stumpwm:current-head))
