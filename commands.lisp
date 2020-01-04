(ql:quickload :cl-utilities)
(in-package :stumpwm)

(defvar *swap-selected-frame* nil "Swapping frames yeah!")
(defvar *home* (getenv "HOME") "THe home directory")
(defvar *gap-resize-increment* 1 "Number of pixels to increment by when interactively resizing gaps.")

(defparameter *log-menu*
  '(("STUMP"
     ("stumpwm.log" "~var/log/stumpwm.log"))
    ("XORG"
     ("Xorg.0.log" "/var/log/Xorg.0.log"))
    ("Package Manager"
     ("pacman.log" "/var/log/pacman.log"))))

(defparameter *quick-slot-menu*
  '(("STUMP Window Manager"
     ("Eval" "eval")
     ("Describe Function" "describe-function")
     ("Describe Command" "describe-command")
     ("Describle Variable" "describe-variable")
     ("Describle Key" "describe-key")
     ("List Commands" "commands")
     ("Save Layout"
      ("Save Desktop""dump-desktop-to-file")
      ("Save Group" "dump-group-to-file")
      ("Save Screen" "dump-screen-to-file"))
     ("Load Layout" "restore-from-file")
     ("Reload Configuration" "loadrc")
     ("Stumpish" "stumpish-term")
     ("Full Screen With Modeline" "fullscreen-with-modeline")
     ("Resize Frame" "iresize"))
    ("EMACS"
     ("Open Stump Configuration Directory" "emacs-edit-stumpwm-config")
     ("Open Compton Configuration" "emacs-compton")
     ("Open Projects Directory" "emacs-projects")
     ("Open Quick Lisp Projects Directory" "emacs-quicklisp")
     ("Open Emacs Configuration" "emacs-edit-config")
     ("Open a File in EMACS" "emacs-entry")
     ("Open Devices Directory" "emacs-devices"))
    ("Display Manager"
     ("Set Random Desktop Image" "set-desktop-picture-full-screen")
     ("Load Xresources" "load-rez")
     ("Refresh Modeline" "refresh-stuff")
     ("Reset Cursor" "reset-xcursor")
     ("Start Compton and Flashfocus" "start-compton-and-flashfocus")
     ("Stop Compton and Flashfocus" "stop-compton-and-flashfocus"))
    ("Audio Manager"
     ("CMUS" "cmus")
     ("LX Music" "lxmusic"))
    ("Net Tools"
     ("Label" "Command"))
    ("Documentation"
     ("Man Page" "manpage")
     ("My StumpWM Config" "my-config-manual")
     ("Info Page" "info-page"))
    ("System Tools"
     ("Open Simple Terminal (st)" "st")
     ("Open Root Simple Terminal (st)" "su-st")
     ("Set CPU to Performance" "set-performance")
     ("Set CPU to Powersave" "set-powersave")
     ("Open Software Manager" "software-manager")
     ("Open IFTOP" "iftop")
     ("Open LX Apperance" "lxlook")
     ("Open Glances" "glances")
     ("Open File Manager" "pcmanfm")
     ("Open XFCE-Taskmanager" "xfce-task")
     ("Opne Gnome Disks" "gpart"))
    ("Web"
     ("Run Geary Email" "geary-email")
     ("Run Firefox" "run-firefox")
     ("IMBD" "imbd")
     ("Duck Duck Go" "ddg")
     ("Youtube" "youtube")
     ("BBS ArcvhLinux" "bbs")
     ("Wikipedia" "wikipedia")
     ("Arch Linux WIKI" "awiki")
     ("last FM" "last.fm")
     ("King James Bible" "kjv")
     ("OSHA" "osha")
     ("Dotshare" "dotshare")
     ("Google" "google"))
    ("Date and Time" "echo-date")
    ("Quit" "quit")) "Menu items for the quickslot menu")

(defparameter *window-menu-items*
  '(("Window Properties" "list-window-properties")
    ("Window List for Group" "windowlist")
    ("Window List for All" "windowlist-all")
    ("Kill Fucused Window" "kill")
    ("Only (Remove Other Frames)" "only")
     )
  "Menu items for window actions")

(defparameter *frame-menu-items*
  '(("Resize Frame" "iresize")
    ("New Vertical Split" "vsplit")
    ("New Horizontal Split" "hsplit")
    ("Remove Frame" "remove"))
  "Menu items for frame actions")

(defparameter *group-menu-items*
  '(("New Group" "gnewbg")
    ("Previous Group" "gother")
    ("Kill Current Group" "gkill")

    ) "Menu items for group actions")

;; Define Commands
(defcommand start-swank-server () () "Start the swank server."
	    (if (= *swank-on* 0)
		(progn
		  (setf *swank-on* 1)
		  (swank-loader:init)
		  (swank:create-server :port 4004
				       :style swank:*communication-style*
				       :dont-close t)
		  (message "Swank Server Started"))
		(message "Swank server is Already Running")))

(defcommand start-swm-gaps () () "Start StumpWM Gaps"
	    (if (= *swm-gaps-on* 0)
		(progn
		  (setf *swm-gaps-on* 1)
		  (swm-gaps:toggle-gaps)
		  (message "StumpWM Gaps Started."))
		(message "StumpWM Gaps Already Running.")))

(defcommand get-cpu-speed (title) () "Get current Speed of CPU"
	    (my-message title (format nil "~a" (run-shell-command "get-cpu-speed.bash" 1))))

(defcommand set-performance () () "Set all cores to max speed."
	    (run-shell-command "sudo cpupower frequency-set -g performance")
	    (get-cpu-speed "Set Performance"))

(defcommand set-powersave () () "Set all cores to power save speed."
	    (run-shell-command "sudo cpupower frequency-set -g powersave")
	    (get-cpu-speed "Set Powersave"))

(defcommand start-modeline () () "Start StumpWM Modeline"
	    (unless (stumpwm::head-mode-line (current-head))
	      (toggle-mode-line (current-screen) (current-head))))

(defcommand start-which-key () () "Start Which key Mode"
	    (if (= *which-key-on* 0)
		(progn
		  (setf *which-key-on* 1)
		  (which-key-mode)
		  (message "Which Key Mode Started."))
		(message "Which Key Mode Alreedy Running.")))

(defcommand swank (&optional port) () "Start a swank server"
  (setf stumpwm:*top-level-error-action* :break)
  (swank:create-server :port (or port 4004)
		       :coding-system "utf-8"
		       :style swank:*communication-style*
		       :dont-close t)
  (message "Starting swank"))

(defcommand showlog (logfile) ((:string "Logfile: ")) "Show log"
            (run-shell-command (format nil "~A -e tail -f ~A" *terminal* logfile)))

(defcommand logmenu () () "Display menu with log files"
            (labels 
              ((pick (options)
                     (let ((selection (select-from-menu (current-screen) options "")))
                       (cond
                         ((null selection)
                          (throw 'error "Abort"))
                         ((stringp (second selection))
                          (second selection))
                         (t
                           (pick (cdr selection)))))))
              (let ((choice (pick *log-menu*)))
                (run-commands (format nil "showlog ~A" choice)))))

(defcommand window-menu () () ""
	    (menu-cmd *window-menu-items*))

(defcommand frame-menu () () ""
	    (menu-cmd *frame-menu-items*))

(defcommand group-menu () () ""
	    (menu-cmd *group-menu-items*))

(defcommand quickslot () () "A Menu for Commonly used commands and opening configuration files"
	    (menu-cmd *quick-slot-menu*))

(defcommand my-message (command-name text) () "For capturing messages from commands." 
	    (let ((*message-window-gravity* :center)
		  (*message-window-padding* 50)
		  (*message-window-y-padding* 30)
		  (*timeout-wait* 10))
	      (message "^B^2 ~a: ^B^5 ~a" command-name text)))

(defcommand menu-cmd (menu-items) () "Command for opening a menu in MENU-ITEMS"
            (labels 
              ((pick (options)
                     (let ((selection (select-from-menu (current-screen) options "")))
                       (cond
                         ((null selection)
                          (throw 'error "Abort"))
                         ((stringp (second selection))
                          (second selection))
                         (t
                           (pick (cdr selection)))))))
              (let ((choice (pick menu-items)))
                (run-commands choice))))

(defcommand colon1 (&optional (initial "")) (:rest) "Prompt the user for an interactive command. The first arg is an optional initial contents."
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

(defcommand load-rez () () "Load Xresources."
	    (run-shell-command "xrdb ~/.Xresources")
	    (my-message "Load Xresources" "Loaded Xresources file."))

(defcommand reset-xcursor () () "Reset the xursor from lightDM."
	     (run-shell-command "xsetroot -cursor_name left_ptr"))

(defcommand set-display-size () () "Sets the Default screen size to 1920x1080."
	     (run-shell-command "~/.screenlayout/default.sh"))

(defcommand set-polkit () () "Set gnome authentication agent."
	     (run-shell-command "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"))

(defcommand set-desktop-picture-full-screen () () "Set the Desktop picture to full screen."
	     (run-shell-command (format nil "feh --bg-scale ~A" (select-random-bg-image))))

(defcommand kill-compton-and-flashfocus () () "Kills compton and flash focus."
	     (run-shell-command "pkill compton && pkill flashfocus"))

(defcommand start-compton () () "Startup Compton with config"
	     (run-shell-command "compton -b -f --config ~/.config/compton.conf"))

(defcommand start-flashfocus () () "Startup Flashfocus with config"
	     (run-shell-command "flashfocus"))

(defcommand start-compton-and-flashfocus () () "Startup Compton and Flashfucus"
	    (run-commands "start-compton" "start-flashfocus"))

(defcommand emacs-client (&optional (item *home*)) () "Startup emacsclient in a frame."
	(run-shell-command (format nil "exec emacsclient -c ~a" item)))

(defcommand emacs-projects () () "Open Projects directory."
	    (emacs-client (format nil "~a/~a"  *home* "Projects")))

(defcommand emacs-compton () () "Open Compton configuration."
	    (emacs-client (format nil "~a/~a"  *home* ".config/compton.conf")))

(defcommand emacs-quicklisp () () "Open Projects directory."
	    (emacs-client (format nil "~a/~a"  *home* "quicklisp")))

(defcommand emacs-edit-config () () "Open emacs config."
	    (emacs-client (format nil "~a/~a" *home* ".emacs.d/config.org")))

(defcommand emacs-edit-stumpwm-config () () "Open stump config."
	    (emacs-client (format nil "~a/~a" *home* ".stumpwm.d/")))

(defcommand emacs-devices () () "Open the Devices Directory"
	    (emacs-client (format nil "~a/~a" *home* "Devices/")))

(defcommand emacs-entry (item) ((:rest "Enter File Name Or Directory: ")) "Interactively enter a file name or path for emacs to open."
	    (emacs-client item))

(defcommand mount-device () () "List all mountable devices and perfoms a sudo mount"
	    (run-shell-command "test-mount.sh"))

(defcommand ranger (&key (directory *home*) (s 0)) () "Startup ranger in user mode or super user mode. If S is 0 then you are in user mode, if S is 1 you are in Super User mode."
	    (message (format nil "~A : ~A" s directory))
	    (if (= s 1)
		(run-shell-command (format nil "exec ~a ~a -e ranger ~a" "sudo" *terminal* directory))
		(run-shell-command (format nil "exec ~a -e ranger ~a" *terminal* directory))))

(defcommand ranger-entry (d) ((:rest "Enter Directory: ")) "Interactively enter a directory to view."
	    (ranger :directory d))

(defcommand ranger-sudo-entry (d) ((:rest "Enter Directory: ")) "Interactively enter a directory to view."
	    (ranger :directory d :s 1))

(defcommand ranger-quicklisp () () "Startup ranger at quicklisp"
	    (ranger :directory (cat (getenv "HOME") "/quicklisp")))

(defcommand ranger-projects () () "Startup ranger at Projects"
	    (ranger :directory (cat (getenv "HOME") "/Projects")))

(defcommand ranger-stumpwm () () "Startup ranger at Stumpwm Configuration directory."
	    (ranger :directory (cat (getenv "HOME") "/.stumpwm.d")))

(defcommand ranger-videos () () "Startup ranger at Videos"
	    (ranger :directory (cat (getenv "HOME") "/Videos")))

(defcommand dump-group-to-file (file) ((:rest "group to file: ")) "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))

(defcommand dump-screen-to-file (file) ((:rest "screen to file: ")) "Dumps the frames of all groups of the current screen to the named file."
  (dump-to-file (dump-screen (current-screen)) file))

(defcommand dump-desktop-to-file (file) ((:rest "desktop to file: ")) "Dumps the frames of all groups of all screens to the named file."
  (dump-to-file (dump-desktop) file))

(defcommand increase-volume () () "Increase the volume"
   (run-shell-command "pactl set-sink-volume 0 +5%"))

(defcommand decrease-volume () () "Decrease the volume"
   (run-shell-command "pactl set-sink-volume 0 -5%"))

(defcommand mute () () "Mute/unmute the volume"
  (run-shell-command "pactl set-sink-mute 0 toggle"))

(defcommand next-song () () "play the next song in playlist"
	    (run-shell-command "cmus-remote --next"))

(defcommand prev-song () () "play the previous song"
	    (run-shell-command "cmus-remote --prev"))

(defcommand stop-song () () "stop the song"
	    (run-shell-command "cmus-remote --stop"))

(defcommand play-song () () "plays/pauses the song"
	    (run-shell-command "cmus-remote --play"))

(defcommand pause-song () () "plays/pauses the song"
	    (run-shell-command "cmus-remote --pause"))

(defcommand paste-x-selection () (:rest) "Universal rat-less X paste."
  (let ((cmd (concatenate 'string "insert " (get-x-selection))))
     (eval-command cmd)))

(defcommand scratchpad-test () ()
	    (scratchpad::scratchpad-float "My Term" "st -e ranger" :top))

(defcommand quit-forget () () "Quit StumpWM without remembering current state."
  (with-open-file (stream *debug-file* :direction :io :if-exists :supersede))
  (cond ((find-group (current-screen) *scratchpad-group-name*)
         (if (eq (current-group) (find-group (current-screen) *scratchpad-group-name*))
             (gkill)
           (progn
             (gnext) (kill-group
                      (find-group (current-screen) *scratchpad-group-name*)
                      (current-group))))))
  (throw :top-level :quit))

(defcommand manpage (command) ((:rest "Man: ")) "Opens a man page"
  (run-shell-command (format nil "~a -e man ~a" *terminal* command)))

(defcommand info-page (command) ((:rest "Info: ")) "Opens an info document"
  ;; TODO - Add rofi search:
  (run-shell-command (format nil "~a -e info ~a" *terminal* command)))

(defcommand my-config-manual () () "Opens my config manual page"
	    (run-shell-command (format nil "~a -e man -l ~a" *terminal* "~/.stumpwm.d/stumpwm-config.1")))

(defcommand uaml () () "Update the mode-line."
  (update-all-mode-lines))

(defcommand new-shell-script () () "Open or create a new shell script."
	    (run-shell-command "exec new-shell-script.sh $(ls ~/bin | rofi -dmenu -p 'Pick or Create a Script' -show)" ))

(defcommand cmus () () "Raise or rum cmus Music Player"
	    (run-or-raise-prefer-title (format nil "exec ~A -t CMUS -e cmus" *terminal*) "cmus..."))

(defcommand lxmusic () () "Run or raise LXMusic Player"
	    (run-or-raise-prefer-title "lxmusic" "LXMusic..."))

(defcommand pcmanfm () () "Run File Manager"
	    (run-or-raise-prefer-title "pcmanfm" "Title"))

(defcommand lxlook () () "Run or raise LX Apperance"
	    (run-or-raise-prefer-title "lxappearance" "Customize Look and Feel"))

(defcommand app-menu () () "Run Rofi drum app menu."
	    (run-shell-command "exec rofi -threads 0 -show-icons -modi drun -show"))

(defcommand run-firefox () () "Run Firefox"
	(run-or-raise-prefer-title "firefox" "Firefox"))

(defcommand rofi-window-menu () () "Run Rofi Window list menu."
	    (run-shell-command "exec rofi -threads 0 -show-icons -modi window -show"))

(defcommand rofi-bang () () "Run Rofi Bang script which includes Apps, Windows, Bookmarks, Kill-App, Move Here, and Power."
	    (run-shell-command "exec rofi-bang.bash"))

(defcommand firefox () () "Run firefox with specific web address."
	    (run-or-raise-prefer-group (colon1 "exec firefox http://www.") "Firefox"))

(defcommand secure-shell () () "Create a ssh connection to a remote host."
	    (run-shell-command (colon1 "exec st -e ssh ")))

(defcommand stumpish-term () () "Create a stumpish terminal."
	    (run-or-raise-prefer-title (format nil "exec ~A -e stumpish" *terminal*) "stumpish"))

(defcommand geary-email () () "Geary email browser for gmail."
	    (run-or-raise-prefer-title "geary" "Geary"))

(defcommand xfce-task () () "Open XFCE4-Taskmanager"
	    (run-or-raise-prefer-title "xfce4-taskmanager" "Task Manager"))

(defcommand rofi-all-windows () () "Show all open applications." (rofi "window"))

(defcommand rofi-window-current-group () () "Show all open applications in Current Desktop." (rofi "windowcd"))

(defcommand rofi-theme-selector () () "Change Rofi Theme."
	    (run-shell-command "exec rofi-theme-selector"))

(defcommand refresh-stuff () () "Refresh the mode line."
  (run-commands "mode-line" "mode-line"))

(defcommand software-manager () () "Install software."
	    (run-shell-command "exec pamac-manager"))

(defcommand software-updater () () "Update software."
	    (run-shell-command "exec pamac-updater"))

(defcommand rotate-windows () () "Rotate all windows within a group to the master frame."
  (let* ((frames (stumpwm::head-frames (current-group) (current-head)))
         (win (stumpwm::frame-window (car (last frames)))))
    (shift-windows-forward frames win)))

(defcommand windows-left-right () () "Open windows side by side"
  (run-commands "only" "vsplit"))

(defcommand windows-up-down () () "Open windows up and down"
  (run-commands "only" "hsplit"))

(defcommand iftop () () "Open IFTOP, to monitor network traffic."
	    (run-shell-command (format nil "exec ~A -e sudo iftop" *terminal*)))

(defcommand st () () "Open Simple Terminal"
	    (run-shell-command "st"))

(defcommand su-st () () "Open Root Simple Terminal"
	    (run-shell-command (format nil "exec ~A sudo zsh" *terminal*)))

(defcommand gpart () () "Open Gnome Disks"
	    (run-shell-command "gnome-disks"))

(defcommand glances () () "Open Glances, to monitor system and processes."
	    (run-shell-command (format nil "exec ~A -e glances" *terminal*)))

(defcommand go-group (n) ((:number "Go to group: ")) "Go to selected group, or back to last used one"
            (if (= (slot-value (current-group) 'number) n)
              (gother)
              (run-commands (format nil "gselect ~A" n))))

(defcommand toggle-split () () "Toggle horizontal/vertical split"
            (let* ((group (current-group))
                   (cur-frame (tile-group-current-frame group))
                   (frames (group-frames group)))
              (if (eq (length frames) 2)
                (progn (if (or (neighbour :left cur-frame frames)
                               (neighbour :right cur-frame frames))
                         (progn
                           (only)
                           (vsplit))
                         (progn
                           (only)
                           (hsplit))))
                (message "Works only with 2 frames"))))

(defcommand swap-windows (&optional (frame (tile-group-current-frame (current-group)))) () "Swap to windows. Invoke once to tag, twice to switch selected window with tagged one"
            (if *swap-selected-frame*
              (progn
	             (let ((win1 (frame-window *swap-selected-frame*))
	                   (win2 (frame-window frame)))
                  (when win1 (pull-window win1 frame))
                  (when win2 (pull-window win2 *swap-selected-frame*)))
                (setf *swap-selected-frame* nil))
              (setf *swap-selected-frame* frame)))

;; (defcommand keepass() ()
;;   (run-or-raise-float  "keepass" "util" '(:class "KeePass2"))(message "keepass"))


;; (defun run-or-raise-float (cmd target-group props &optional (all-groups *run-or-raise-all-groups*)
;;                                  (all-screens *run-or-raise-all-screens*))
;;   "Like run-or-raise, but preselect target group.  Only necessary for float groups."
;;   (labels
;;       ((goto-win (win)
;;          (let* ((group (window-group win))
;;                 (frame (window-frame win))
;;                 (old-frame (tile-group-current-frame group)))
;;            (focus-all win)
;;            (unless (eq frame old-frame)
;;              (show-frame-indicator group)))))
;;     (let* ((matches (find-matching-windows props all-groups all-screens))
;;            (other-matches (member (current-window) matches))
;;            (win (if (> (length other-matches) 1)
;;                     (second other-matches)
;;                     (first matches))))
;;       (if win
;;           (if (eq (type-of (window-group win))
;;                   'stumpwm.floating-group:float-group)
;;               (focus-all win)
;;               (goto-win win))
;;           (progn
;; 	    (gnew-float target-group)
;; 	    (run-shell-command cmd))))))
(defcommand urxvt () () "Start an urxvt instance or switch to it, if it is already running."
  (run-or-raise "urxvt"'(:title "urxvt")))

(defcommand rxvt-quake () () "Toggle rxvt-quake window."
  (if (equal (window-title (current-window)) "urxvt")   ; if the current window is quake
      (progn
	(other-window)    ; switch back to window quake was called from
	(select-window-by-number *real-other-window*)  ; switch to the 'real' "other-window"
	(other-window))  ; switch back to the original window - this way after quake finishes, the original configuration is restored
      (progn          ; otherwise, if the current window is NOT quake
      (other-window)   ; first switch the current "other-window"
	(if (not (equal (window-title (current-window)) "urxvt")) ; if the current
								  ; "other-window" is
								  ; quake itself, do
								  ; nothing
	    (setf *real-other-window* (window-number (current-window)))) ; otherwise store the window-number of the current other-window
	(other-window) ; switch back to the window originally called from
	(urxvt)))) ; run-or-raise urxvt

(defcommand echo-colors-brief () () "Output a brief list of currently defined colors."
  (echo-string (current-screen) "BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white^n NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white^n"))

(defcommand echo-test () () "Output a test message"
	    (echo-string (current-screen) "Test ^0*Black."))

(defcommand fullscreen-with-modeline () () "fake version of fullscreen that retains border and mode-line"
  (let ((group-file (format nil "/tmp/stumpwm-group-~a" (group-name (current-group)))))
    (if (null (cdr (head-frames (current-group) (current-head))))
        (restore-from-file group-file)
        (progn
          (dump-group-to-file group-file)
(only)))))

(defcommand fix-height () () "Resize the current frame height so that the current window's sides are touching the frame sides if the current window is of fixed proportions (e.g. mplayer)"
  (let* ((window (current-window))
         (maxsize (window-maxsize-p window)))
    (when maxsize
      (let* ((frame (window-frame window))
             (frame-width (frame-width frame))
             (frame-height (frame-height frame))
             (new-height (truncate (/ frame-width maxsize)))
             ;; not sure why it's too short w/o 15
             (difference (+ 15 (- new-height frame-height))))
        (resize 0 difference)))))

(defcommand fix-width () () "Resize the current frame width so that the current window's sides are touching the frame sides if the current window is of fixed proportions (e.g. mplayer)"
  (let* ((window (current-window))
         (maxsize (window-maxsize-p window)))
    (when maxsize
      (let* ((frame (window-frame window))
             (frame-width (frame-width frame))
             (frame-height (frame-height frame))
             (new-width (truncate (* frame-height maxsize)))
             ;; not sure why it's too short w/o 15
             (difference (- new-width frame-width)))
        (resize difference 0)))))

(defcommand gmove-and-follow (to-group) ((:group "To Group: ")) "Move the current window to the specified group and go there."
  (when (and to-group
             (current-window))
    (move-window-to-group (current-window) to-group)
(switch-to-group to-group)))

(defcommand run-shell-command (cmd &optional collect-output-p) ((:shell "Execute: ")) "Run the specified shell command. If @var{collect-output-p} is @code{T} then run the command synchronously and collect the output."
  (if collect-output-p
    (run-prog-collect-output *shell-program* "-c" cmd)
    (run-prog *shell-program* :args (list "-c" cmd) :wait nil)))
;(defcommand-alias exec run-shell-command)

;; TODO -- Modify for resizing gaps
(defun abort-resize-p () "Resize is only available if there's more than one frame."
  (when (single-frame-p)
    (message "There's only 1 frame!")
    t))

(defcommand resize-gaps (the-size) ((:number "Enter Number: ")) "Resize gaps around windows interactively using hjkl or arrow keys."
	    (setf *useless-gaps-size* the-size)
	    (if (equal *useless-gaps-on* T)
		(progn
		  (gaps)
		  (gaps))
		  (gaps)))
