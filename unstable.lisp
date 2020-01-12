;;This file is in place for unstable functions and commands.  It should be loaded at the end of init.lisp.
(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-tutorial
  (:use :gtk :gdk :gdk-pixbuf :gobject
   :glib :gio :pango :cairo :cffi :common-lisp))

(in-package :gtk-tutorial)

(defun rel-path (filename)
  (let ((system-path (asdf:system-source-directory :cl-cffi-gtk-example-gtk)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun example-simple-window ()
  (within-main-loop
    (let (;; Create a toplevel window.
          (window (gtk-window-new :toplevel)))
      ;; Signal handler for the window to handle the signal "destroy".
      (g-signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))
      ;; Show the window.
      (gtk-widget-show-all window)))
    (join-gtk-main))

(defun example-simple-message ()
  (let ((response))
    (within-main-loop
      (let ((dialog (make-instance 'gtk-message-dialog
                                   :message-type :info
                                   :buttons :ok
                                   :text "Info Message Dialog"
                                   :secondary-text
                                   (format nil
                                           "This is a message dialog of type ~
                                            :info with a secondary text."))))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
                          (lambda (dialog response-id)
                            (setf response response-id)
                            (gtk-widget-destroy dialog)))
        (gtk-widget-show dialog)))
    (join-gtk-main)
    (format t "Back from message dialog with response-id ~A~%" response)))

(defun example-simple-file-chooser-dialog ()
  (let ((file-name nil))
    (within-main-loop
      (let ((dialog (gtk-file-chooser-dialog-new "Open File in Emacs"
                                                 nil
                                                 :open
                                                 "gtk-cancel" :cancel
                                                 "gtk-open" :accept)))
        ;; Signal handler for the dialog to handle the signal "destroy".
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            ;; Quit the main loop and destroy the thread
                            (leave-gtk-main)))
        ;; Signal handler for the dialog to handle the signal "response".
        (g-signal-connect dialog "response"
           (lambda (dialog response-id)
             ;; Check the response id from the file chooser dialog
             (when (eql response-id
                        ;; Convert the symbol :accept to the number value.
                        (foreign-enum-value 'gtk-response-type :accept))
               ;; Get the file name and store it.
	       (setf file-name (gtk-file-chooser-get-current-folder dialog)))
;               (setf file-name (gtk-file-chooser-get-filename dialog)))
             ;; Destroy the dialog.
             (gtk-widget-destroy dialog)))
        ;; Show the dialog.
        (gtk-widget-show dialog)))
    ;; Wait until the dialog is finished.
    (join-gtk-main)
    (when file-name
      ;;(format t "~A~&" file-name))))
      (stumpwm:emacs-entry file-name))))

(defun example-dialog-ui ()
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-from-file builder (rel-path "dialog.ui"))
      (format t "DEBUG: ~%" )
      (let ((dialog (gtk-builder-get-object builder "dialog")))
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
	(gtk-widget-show dialog))))
  (join-gtk-main))
