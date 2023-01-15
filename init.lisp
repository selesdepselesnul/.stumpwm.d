(in-package :stumpwm)
(require :str)
(require :cl-ppcre)
(require :swank)

(setq *custom-modules-path* "~/.stumpwm.d/custom-modules")

(defun load-custom-module (custom-module)
  (load  (concatenate 'string *custom-modules-path* "/" custom-module)))

(load-custom-module "util")
(load-custom-module "font")
(load-custom-module "repl-server")
(load-custom-module "battery")
(load-custom-module "disk")
(load-custom-module "global-var")
(load-custom-module "date-time")
(load-custom-module "uptime")
(load-custom-module "command-and-key")

(setf *screen-mode-line-format*
      (list "" '(:eval (date-time))
            " | " '(:eval (read-bat-capacity))
            " | " '(:eval (read-bat-status))
            " | " '(:eval (disk-usage))
            " | " '(:eval (check-uptime))))


;; turn on/off the mode line for the current head only.
(toggle-mode-line (current-screen) (current-head))


