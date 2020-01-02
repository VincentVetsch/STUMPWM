(in-package :stumpwm)
(defun xclose-window (window display)
  "Close created WINDOW on specified DISPLAY."
  (xlib:destroy-window window)
  (xlib:close-display display))

(defun button-test (width height btn-text &optional (host ""))
  "A test to create a window with a button with text."
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (drw-context (xlib:create-gcontext
		       :drawable root-window
		       :cap-style :round
		       :foreground (parse-integer "dd00ff" :radix 16)
		       :background (parse-integer "002200" :radix 16)))
	 (inner-context (xlib:create-gcontext
		       :drawable root-window
		       :cap-style :round
		       :foreground (parse-integer "ddaaff" :radix 16)
		       :background (parse-integer "002200" :radix 16)))
	 (font-context (xlib:create-gcontext
			:drawable root-window
			:foreground(parse-integer "ffffff" :radix 16) 
			:background black))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background black ;(parse-integer "004400" :radix 16) ;black
		     :event-mask (xlib:make-event-mask :exposure
						       :enter-window
						       :button-press))))
    (describe font-context)
    (describe drw-context)
    (xlib:map-window my-window)
    ;(xlib:display-finish-output display)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:exposure (count)
		 (format t "The Count is: ~A~%" count)
		   (xlib:draw-line my-window inner-context 0 55 640 55)
		   (xlib:draw-line my-window inner-context 0 450 640 450)
		   (xlib:draw-rectangle my-window drw-context 440 470 100 50 nil)
		   (xlib:draw-rectangle my-window inner-context 445 475 90 40 t)
		   (xlib:draw-arc my-window
				  drw-context
				  200
				  200
				  100
				  100
				  (degrees-to-radians 360)
				  (degrees-to-radians 360)
				  nil)
		   (xlib:draw-arc my-window
				  drw-context
				  100
				  200
				  100
				  100
				  (degrees-to-radians 90)
				  (degrees-to-radians 180)
				  nil)
		   (xlib:draw-arc my-window
				  drw-context
				  300
				  200
				  100
				  100
				  (degrees-to-radians 270)
				  (degrees-to-radians 180)
				  nil)
		   (xlib:draw-glyphs my-window font-context 20 50 btn-text)
		   (format t "Foreground: ~A~%" white)
		   (format t "Background: ~A~%" black)
		   (format t "It should be here ~A~%" btn-text))
      (:enter-notify ()(format t "Entered Window~%"))
      (:button-press () t))
    
    (xclose-window my-window display)))

(defun graphic-x (width height across down &optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (green (xlib:alloc-color
		 (xlib:window-colormap root-window)
		 'green))
	 (blue (xlib:alloc-color
		 (xlib:window-colormap root-window)
		 'blue))
	 (red (xlib:alloc-color
		 (xlib:window-colormap root-window)
		 'red))
	 (top-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background black
		     :event-mask (xlib:make-event-mask :key-press
						       :button-press)))
	 (red-window (xlib:create-window
		     :parent top-window
		     :x across
		     :y 0
		     :width (truncate width 4)
		     :height (truncate height 4)
		     :background red
		     :event-mask (xlib:make-event-mask :button-press)))
	 (green-window (xlib:create-window
		     :parent top-window
		     :x 0
		     :y down
		     :width (truncate width 4)
		     :height (truncate height 4)
		     :background green
		     :event-mask (xlib:make-event-mask :button-press)))
	 (blue-window (xlib:create-window
		     :parent top-window
		     :x across
		     :y down
		     :width (truncate width 4)
		     :height (truncate height 4)
		     :background blue
		     :border-width 5
		     :border white
		     :event-mask (xlib:make-event-mask :button-press))))
    (xlib:map-window top-window)
    (xlib:map-window red-window)
    (xlib:map-window green-window)
    (xlib:map-window blue-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:button-press (window)
		     (cond ((eq window red-window)
			    (format t "Red Window Destroyed~%")
			    (xlib:destroy-window red-window)
			    nil)
			   ((eq window green-window)
			    (xlib:destroy-window blue-window)
			    nil)
			   ((eq window blue-window)
			    (xlib:destroy-window green-window)
			    nil)
			   (t t)))
      (:key-press ()
		  (xlib:circulate-window-down top-window)
		  nil))
    (xlib:destroy-window top-window)
    (xlib:close-display display)))
