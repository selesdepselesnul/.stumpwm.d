(in-package :stumpwm)

;; Web jump (works for google and youtube)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
               (substitute #\+ #\Space search)
               (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "google-chrome-stable http://www.google.com/search?q=")
(make-web-jump "youtube" "google-chrome-stable https://www.youtube.com/results?search_query=")

(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "c") "exec xfce4-terminal")
