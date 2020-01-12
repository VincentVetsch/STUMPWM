;; -*-lisp-*-
;;; Load extra packages
(ql:quickload :cl-utilities)
(ql:quickload :clx-truetype)
(ql:quickload :clx)
(ql:quickload :xembed)
(ql:quickload :anaphora)
(ql:quickload :serapeum)
(ql:quickload :alexandria)
(ql:quickload :swank)
(defpackage hfj
  (:use :cl :stumpwm :anaphora))
(in-package :hfj)

(require :stumpwm)
(in-package :stumpwm)
(defvar *which-key-on* 0 "In the beginning which-key is off")
(defvar *swm-gaps-on* 0 "In the beginning swm-gaps is off")
(defvar *swank-on* 0 "In the beginning swank is off")
(defvar *terminal* "st" "The default Terminal Emulator.")
(defvar *browser* "firefox" "The Default web browser.")
(defparameter *hostname* (string-trim '(#\Newline) (run-shell-command "hostname" t)) "The host name of current machine.")
(defvar *home-dir* nil "The home Direcctory form Stumpmwm")
(defvar *stump-dir* nil "Stump Data directory")
(defvar *load-dir* nil "Load files")
(defvar *undo-data-dir* nil "Undo Directory")
(defvar *background-image-path* nil "Image directory.")

(load-module "windowtags")
(load-module "swm-gaps")
(load-module "ttf-fonts")
;;(load-module "stumptray")
;;(load-module "winner-mode")
(load-module "globalwindows")
;;(load-module "undocumented")
(load-module "command-history")
;;;Load Init Files 
;; TODO - Create a load loop
(load "~/.stumpwm.d/startup.lisp")
(load "~/.stumpwm.d/macros.lisp")
(load "~/.stumpwm.d/functions.lisp")
(load "~/.stumpwm.d/gtk-ui.lisp")
(load "~/.stumpwm.d/layout.lisp")
(load "~/.stumpwm.d/window.lisp")
(load "~/.stumpwm.d/window1.lisp")
(load "~/.stumpwm.d/scratchpad.lisp")
(load "~/.stumpwm.d/visual.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/mode-line.lisp")
(load "~/.stumpwm.d/vi-keys.lisp")
(load "~/.stumpwm.d/constants.lisp")
(defvar *network-interfaces* (get-interface)
  "All interfaces and dotted IP Addresses")

(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1"
      *home-dir* (make-pathname :directory (format nil "~A/.stumpwm.d" (getenv "HOME")))
      *data-dir* (make-pathname :directory (format nil "~A/.stumpwm.d/data" (getenv "HOME")))
      *background-image-path* (make-pathname :directory (format nil "~A/Pictures" (getenv "HOME")))
      *stump-dir*     (merge-pathnames (make-pathname :directory '(:relative "stumpwm")) *home-dir*)
      *load-dir*      (merge-pathnames (make-pathname :directory '(:relative "lisp")) *stump-dir*)
      *undo-data-dir* (make-pathname :directory "/dev/shm/.1000")
      *shell-program* "/bin/sh"
      *suppress-frame-indicator* t
      ;*input-completion-type* :fuzzy
      *message-window-padding* 20
      *message-window-y-padding* 10
      *mouse-focus-policy* :click
      *message-window-gravity* :top ;;:center
      *input-window-gravity* :top ;;:center
      *transient-gravity* :top
      *transient-border-width* 2
      *input-history-ignore-duplicates* t
      *group-format*    "^b%n:^B%t"
      *window-format*   "^b%n:^B%20t%m"
      *time-modeline-string* "%H:%M:%S %d/%m/%Y"
      *window-info-format* (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t")
      ;*scratchpad-group-name* ".scratchpad"
      *debug-level* 0
      swm-gaps:*inner-gaps-size* 2
      swm-gaps:*outer-gaps-size* 6
      *mode-line-click-hook*
            (list #'(lambda (mode-line button x y)
		(declare (ignore mode-line x y))
		(cond ((eq button 3) (run-commands "next"))
		      ((eq button 2) (run-commands "grouplist"))
		      ((eq button 4) (run-commands "gnext"))
		      ((eq button 5) (run-commands "gprev"))
		      ((eq button 1) (run-commands "prev"))))))

;; Start Up groups ;;
(run-commands
 "start-swank-server"
 "start-swm-gaps"
 "start-modeline"
 "start-which-key"
 "gnewbg WWW"
 "gnewbg System"
 "gnewbg Emacs"
 "gnewbg Email")
;; setup debug-file variable for referencing (e.g. quitting) purposes.
(redirect-debug *debug-file*)

;; Load the Keymap and web jump
(load "~/.stumpwm.d/key-map.lisp")
(load "~/.stumpwm.d/web-jump.lisp")
;;; Define window placement policy...

(clear-window-placement-rules)
;; Directories

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
(define-frame-preference "Default"
  (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "WWW"
    (1 t t :restore "firefox-dump" :class "Firefox")
    (0 t t :create "Firefox-dump" :class "Firefox")
    (0 t nil :class "Firefox"))

(define-frame-preference "Shareland"
  (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))

(define-frame-preference "Emacs"
  (1 t t :restore "emacs-editing-dump" :title "...xdvi")
  (0 t t :create "emacs-dump" :class "Emacs"))

(load "~/.stumpwm.d/unstable.lisp")
