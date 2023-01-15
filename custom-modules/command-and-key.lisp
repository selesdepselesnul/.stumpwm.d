(in-package :stumpwm)
(require :str)
(require :cl-ppcre)


(define-key *top-map* (kbd "s-F5") "refresh")
(define-key *top-map* (kbd "s-.") "exec")

(setq *default-terminal* "lxterminal")

(defun run-on-terminal (command)
  (concatenate 'string *default-terminal* " -e " command))

(make-custom-key selesdepselesnul/lxterminal "lxterminal" *top-map* "s-c")
(make-custom-key selesdepselesnul/vs-code "code" *top-map* "s-e")
(make-custom-key selesdepselesnul/htop (run-on-terminal "htop") *top-map* "s-p")
(make-custom-key selesdepselesnul/vlc "vlc" *top-map* "s-v")
(make-custom-key selesdepselesnul/okular "okular" *top-map* "s-r")
(make-custom-key selesdepselesnul/postman "postman" *top-map* "s-u")
(make-custom-key selesdepselesnul/google-chrome "google-chrome-stable" *top-map* "s-B")
(make-custom-key selesdepselesnul/firefox "firefox" *top-map* "s-o")
(make-custom-key selesdepselesnul/thunar "thunar" *top-map* "s-f")

