(asdf:load-system :cl-cffi-gtk)

(defpackage :gtk-ui
  (:use :gtk :gdk :gdk-pixbuf :gobject
	:glib :gio :pango :cairo :cffi :common-lisp)
  (:export #:file-chooser-dialog))

(in-package :gtk-ui)

(defun file-chooser-dialog (cmd window-title) "Open a File Chooser with CMD (Command) and TITLE (Dialog Title)"
  (let ((file-name nil))
    (within-main-loop
      (let ((dialog (gtk-file-chooser-dialog-new window-title
                                                 nil
                                                 :open
                                                 "gtk-cancel" :cancel
                                                 "gtk-open" :accept)))
        (g-signal-connect dialog "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect dialog "response"
           (lambda (dialog response-id)
             (when (eql response-id
                        (foreign-enum-value 'gtk-response-type :accept))
		   (setf file-name (gtk-file-chooser-get-filename dialog)))
             (gtk-widget-destroy dialog)))
        (gtk-widget-show dialog)))
    (join-gtk-main)
    (when file-name
      (format t "~A~&" file-name)
      (funcall cmd file-name))))
