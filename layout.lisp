(in-package :stumpwm)

(defvar vv-frames1 nil)

(defun vv-set-frames (frames &optional (populatep t))
  "Display FRAMES in the current group.
The first frame will become the current one and will contain the current
window.  If POPULATEP is nil, do not populate the rest frames with
windows."
  (let* ((screen     (current-screen))
         (group      (screen-current-group screen))
         (head       (current-head group))
         (cur-window (group-current-window group))
         (cur-frame  (first frames)))
    (mapc (lambda (w)
            (setf (window-frame w) cur-frame))
          (group-windows group))
    (mapc (lambda (f)
            (setf (frame-window f) nil))
          (rest frames))
    (setf (frame-window cur-frame) cur-window
          (tile-group-frame-head group head) frames)
    (when populatep
      (populate-frames group))

    (focus-frame group cur-frame)
    (update-decoration cur-window)
    (sync-frame-windows group cur-frame)))

(defun vv-make-frames1 ()
  "Return a frame layout (list of frames) for `vv-frames1'."
  (let* ((screen    (current-screen))
         (s-width   (screen-width screen))
         (s-height  (screen-height screen))
         (f0-width  (/ s-width 2))
         (f0-height (* 3 (/ f0-width 4)))
         (f0 (make-frame
              :number 0
              :x 0 :y 0
              :width f0-width
              :height f0-height))
         (f1 (make-frame
              :number 1
              :x 0 :y f0-height
              :width f0-width
              :height (- s-height f0-height)))
         (f2 (make-frame
              :number 2
              :x f0-width :y 0
              :width (- s-width f0-width)
              :height s-height)))
    (list f0 f2 f1)))

(defcommand vv-frames1 (&optional (populatep t)) ()
  "Show layout of 3 frames with one frame having 4/3 ratio."
  (vv-set-frames (or vv-frames1
                     (setf vv-frames1 (vv-make-frames1)))
                 populatep))
