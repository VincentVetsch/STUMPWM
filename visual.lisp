(in-package :stumpwm)

;; Message window font
(xft:cache-fonts)
(set-font (make-instance
	   'xft:font
	   :family "DejaVu Sans Mono"
	   :subfamily "Bold"
	   :size 10
	   :antialias t))
;; (setf *colors* (list "#1C2028"      ; 0 black
;;                      "#BF616A"      ; 1 red
;;                      "#A3BE8C"      ; 2 green
;;                      "#EBCB8B"      ; 3 yellow
;;                      "#5E81AC"      ; 4 blue
;;                      "#B48EAD"      ; 5 magenta
;;                      "#8FBCBB"      ; 6 cyan
;; 		     "#ECEFF4")) ; 7 white

(set-msg-border-width 2)
(set-normal-gravity :top) ; top for terminals
(set-maxsize-gravity :top) ; top for floating X apps
(set-transient-gravity :top) ; top for save-as/open popups
(setf *colors* (list "#3c4c55"      ; 0 black
                     "#ff6a6a"      ; 1 red
                     "#A3BE8C"      ; 2 green
                     "#dada93"      ; 3 yellow
                     "#83afe5"      ; 4 blue
                     "#7fc1ca"      ; 6 cyan
                     "#9a93e1"      ; 5 magenta
		     "#899ba6"      ; 7 white
		     "#ffffff"      ; 8 User1
		     "#000000"      ; 9 User2
		     ))   
(update-color-map (current-screen))
(set-fg-color "#899ba6")
(set-bg-color "#282A36");;"#3c4c55")
(set-border-color "#3c4c55")
(set-focus-color "#3B4252")
(set-unfocus-color "#232731")
(setf
      *mode-line-foreground-color* "#899ba6"
      *mode-line-background-color* "#001011" ;"#3c4c55"
      *mode-line-border-color* "#3c4c55"
      *bar-med-color* "^B^2" 
      *bar-hi-color* "^B^3"
      *bar-crit-color* "^B^1"
 )

;; Setup Borders and Colors
(setf *normal-border-width* 1
      *maxsize-border-width* 0
      *transient-border-width* 1
      *window-border-style* :thin
      *mode-line-border-width* 0
      *mode-line-pad-x* 0
      *mode-line-pad-y* 0
      *mode-line-position* :top)
(update-color-map (current-screen))
