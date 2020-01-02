(in-package :stumpwm)
(ql:quickload :str)
(ql:quickload :ip-interfaces)
(ql:quickload :clx)
;;(declaim (optimize (speeed 3) (safety 1)))


(defvar *internet-interface* (nth 1 (ip-interfaces:get-ip-interfaces))
 "All applicable information on primary internet interface")
(defvar *all-internet-interfaces* (ip-interfaces:get-ip-interfaces)
  "All applicable informamtion on all internet interfaces")
(defvar *debug-restream* nil)
(defvar *debug-file* (data-dir-file "log" "lisp"))
(defvar *useless-gaps-size* 10)
(defvar *useless-gaps-on* nil)
(defparameter *async-shell* (uiop:launch-program "bash" :input :stream :output :stream) "Setup a Asynchronis Shell")

;(defun draw-button (width hieght color text window display)
;  "Draw a button on a WINDOW with the specified WIDTH HIEGHT and TEXT"
;  )
(defun async-run (command) "Runs a command in Asynchronis mode"
  (write-line command (uiop:process-info-input *async-shell*))
  (force-output (uiop:process-info-input *async-shell*))
  (let* ((output-string (read-line (uiop:process-info-output *async-shell*)))
         (stream (uiop:process-info-output *async-shell*)))
    (if (listen stream)
        (loop while (listen stream)
              do (setf output-string (concatenate 'string
                                                  output-string
                                                  '(#\Newline)
                                                  (read-line stream)))))
    output-string))
(defun show (obj)
  "Use typecase for showing type of OBJ."
  (typecase obj
    (integer "An Integer")
    (float "A Float")
    (string "A String")
    (list "A List")))

(defun degrees-to-radians (degrees)
  "Convert DEGREES to radians."
  (* degrees .01745))

(defun golden-ratio ()
  "Compute the golden raito."
  (/ (+ 1 (sqrt 5)) 2))

(defun my-debug (&rest data)
  (with-open-file (stream (uiop:subpathname* (user-homedir-pathname) "tmp/stumpwm.txt")
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "~&~A" (first data))
    (loop for item in (rest data)
          do (format stream " ~A" item))
    (terpri stream)))

(defun if-file-exists (function file) "Run a FUNCTION on a FILE it if it exists."
  (if (probe-file file)
      (funcall function file)))

(defun file-exists-p (file-name)
  "Check if the FILE-NAME exists."
  (if (probe-file file-name)
      1
      nil))

(defun probe-file-env-paths (name)
  "Probe file across paths in $PATH.  Returns first pathname found or nil."
  (loop for path in (str:split ":" (uiop:getenv "PATH") :omit-nulls t)
thereis (probe-file (merge-pathnames name (make-pathname :directory path)))))

(defun window-cls-present-p (win-cls &optional all-groups)
  "Tell if a window (by class) is present"
  (let ((windows (group-windows (if all-groups (current-screen) (current-group)))))
    (member win-cls (mapcar #'window-class windows) :test #'string-equal)))

(defun run-or-raise-prefer-group (cmd win-cls)
  "If there are windows in the same class, cycle in those. Otherwise call
run-or-raise with group search t."
  (if (window-cls-present-p win-cls)
      (run-or-raise cmd `(:class ,win-cls) nil T)
      (run-or-raise cmd `(:class ,win-cls) T T)))

(defun run-or-raise-prefer-title (cmd win-cls)
  "If there are windows in the same title, cycle in those. Otherwise call
run-or-raise with group search t."
  (if (window-cls-present-p win-cls)
      (run-or-raise cmd `(:title ,win-cls) nil T)
      (run-or-raise cmd `(:title ,win-cls) T T)))

(defun shift-windows-forward (frames win)
  "Exchange windows through cycling frames."
  (when frames
    (let ((frame (car frames)))
      (shift-windows-forward (cdr frames)
                             (stumpwm::frame-window frame))
      (when win
	(stumpwm::pull-window win frame)))))

(defun global (key value)
  (define-key *top-map* (kbd key) value))

(defun cat (&rest strings)
  (apply 'concatenate 'string strings))

(defun colour (key)
  (let ((colours '(:base03 #x002b36
                   :base02 #x073642
                   :base01 #x586e75
                   :base00 #x657b83
                   :blue0 #x373b43
                   :ypnose #x1c2027
                   :ypnosebl #x3e7ef3
                   :ypnosecy #x30a8e0
                   :blue1 #x242931
                   :base2 #xeee8d5
                   :base3 #xfdf6e3
                   :yellow #x99ad6a
                   :orange #xcb4b16
                   :red #xdc322f
                   :magenta #xd33682
                   :violet #x6c71c4
                   :blue #x268bd2
                   :cyan #x87ceeb
		   :dfx #x14db49
		   :black #x000000
                   :green #x8ae234)))
    (getf colours key)))

(defun read-file (file) "Open FILE and read the stream."
  (with-open-file (stream file) (read-line stream)))

(defun update-mode-line () "Update the mode-line sooner than usual."
  (let ((screen (current-screen)))
    (when (screen-mode-line screen)
      (redraw-mode-line-for (screen-mode-line screen) screen))))

(defun rofi (mode) "Setup rofi command string with the specified MODE."
  (run-shell-command (concat "rofi -threads 0 -show-icons -show " mode " -m "
			     (write-to-string (head-number (current-head))))))

(defun color-ping (s)
  (if (equal s "")
      ""
      (let* ((words (cl-ppcre:split "\\s+" s))
             (ping (multiple-value-bind (_ ping)
                       (cl-ppcre:scan-to-strings "([\\.\\d]+) ms" s) (elt ping 0)))
             (color (bar-zone-color (read-from-string ping)
                                    300 700 1000))
             (colored-ping (format nil "^[~A~3D^]" color ping)))
	(cl-ppcre:regex-replace ping s colored-ping))))

(defun read-net (device-name) "Read the IP address for DEVICE-NAME."
  (with-open-file (stream "/proc/net/arp")
    (loop for line = (read-line stream nil 'foo)
          until (eq line 'foo)
       do (if (search device-name line)
	   (print line)))))

(defun ip-to-string (ip) "Takes a list which has the ip address."
  (format nil "~{~A~^.~}" ip))

(defun get-interface () "Return all available network interfaces by name in list form."
  (let ((x (loop :for i :in *all-internet-interfaces* :collect
	      (list (ip-interfaces::ip-interface-name i)
		    (loop :for j
		       :being :the :element :of (ip-interfaces::ip-interface-address i)
		       :collect j)))))
    (loop :for k :in x :collect (list (car k) (ip-to-string (cadr k))))))

(defun save-hash-table-to-file (ht filename) "Save a hash table HT to a FILENAME."
  (let (list)
    (flet ((save-slot (key  value)
	     (push (cons key value) list)))
      (maphash #'save-slot ht))
    (with-open-file (f filename :direction :output )
      (prin1 list f))))

(defun read-hash-table-from-file (filename) "Read a hash table from a FILENAME."
  (let ((ht (make-hash-table)))
    (with-open-file (f filename)
      (dolist (cell (read f))
	(let ((key (car cell))
	      (value (cdr cell)))
	  (setf (gethash key ht) value))))
    ht))

(defun redirect-debug (file) "Redirect *debug-stream* directly to a file."
  (when (typep *debug-restream* 'file-stream)
    (close *debug-restream*))
  (setf *debug-restream* (open file :direction :output :if-exists :append
                         :if-does-not-exist :create)
        *debug-stream* *debug-restream*))

;; setup debug-file variable for referencing (e.g. quitting) purposes.
(redirect-debug *debug-file*)

;; Redefined - with `if`s for *useless-gaps-on*
(defun maximize-window (win) "Maximize the window."
  (multiple-value-bind (x y wx wy width height border stick)
      (geometry-hints win)

    (if *useless-gaps-on*
        (setf width (- width (* 2 *useless-gaps-size*))
              height (- height (* 2 *useless-gaps-size*))
              x (+ x *useless-gaps-size*)
              y (+ y *useless-gaps-size*)))

    (dformat 4 "maximize window ~a x: ~d y: ~d width: ~d height: ~d border: ~d stick: ~s~%" win x y width height border stick)
    ;; This is the only place a window's geometry should change
    (set-window-geometry win :x wx :y wy :width width :height height :border-width 0)
    (xlib:with-state ((window-parent win))
      ;; FIXME: updating the border doesn't need to be run everytime
      ;; the window is maximized, but only when the border style or
      ;; window type changes. The overhead is probably minimal,
      ;; though.
      (setf (xlib:drawable-x (window-parent win)) x
            (xlib:drawable-y (window-parent win)) y
            (xlib:drawable-border-width (window-parent win)) border)
      ;; the parent window should stick to the size of the window
      ;; unless it isn't being maximized to fill the frame.
      (if (or stick
              (find *window-border-style* '(:tight :none)))
          (setf (xlib:drawable-width (window-parent win)) (window-width win)
                (xlib:drawable-height (window-parent win)) (window-height win))
          (let ((frame (window-frame win)))
            (setf (xlib:drawable-width (window-parent win)) (- (frame-width frame)
                                                               (* 2 (xlib:drawable-border-width (window-parent win)))
                                                               (if *useless-gaps-on* (* 2 *useless-gaps-size*) 0))
                  (xlib:drawable-height (window-parent win)) (- (frame-display-height (window-group win) frame)
                                                                (* 2 (xlib:drawable-border-width (window-parent win)))
                                                                (if *useless-gaps-on* (* 2 *useless-gaps-size*) 0)))))
      ;; update the "extents"
      (xlib:change-property (window-xwin win) :_NET_FRAME_EXTENTS
                            (list wx wy
                                  (- (xlib:drawable-width (window-parent win)) width wx)
                                  (- (xlib:drawable-height (window-parent win)) height wy))
                            :cardinal 32))))

(defun reset-all-windows () "Reset the size for all tiled windows"
  (let ((windows (mapcan (lambda (g)
                           (mapcar (lambda (w) w) (sort-windows g)))
                         (sort-groups (current-screen)))))
    (mapcar (lambda (w)
              (if (string= (class-name (class-of w)) "TILE-WINDOW")
                  (maximize-window w))) windows)))

(defcommand gaps () () "Toggle the padding of tiled windows"
            (setf *useless-gaps-on* (null *useless-gaps-on*))

            ;; Following is pseudo code to use hooks
            ;; to do something like change border colors or size
            ;; (if *useless-gaps-on*
            ;;     (run-hook 'frame-gap-on)
            ;;     (run-hook 'frame-gap-off))
(reset-all-windows))

(defun select-random-bg-image () "Select a random image from *background-image-path* and display it on the root window. This is a rewrite of another function to check for errors and allow more than one picture type, as display command will only display valid files anyway."
  (if (ensure-directories-exist *background-image-path*)
    (let ((file-list (directory (make-pathname :defaults *background-image-path*
            :name :wild :type :wild :case :common)))
          (*random-state* (make-random-state t)))
      (namestring (nth (random (length file-list)) file-list)))))

(defun print-list (the-list) "Prints the-list."
  (if (not (null the-list))
      (progn
	(print (car the-list))
	(print-list (cdr the-list)))))

(defun p (the-thing) "Shortcut for print" (print the-thing))

(defun pl (the-list) "Shortcut for print-list" (print-list the-list))

(defun env () "Shortcut for environment" (sb-ext:posix-environ))

(defun cwd () "Shortcut for Current Working Directory." (print *default-pathname-defaults*))

(defun fp (the-name) "shorcut for find package THE-NAME of the package." (find-package the-name))
