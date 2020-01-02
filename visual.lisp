(in-package :stumpwm)

;; Message window font
(xft:cache-fonts)
(set-font (make-instance
	   'xft:font
	   :family "DejaVu Sans Mono"
	   :subfamily "Bold"
	   :size 10
	   :antialias t))
;; 					;(set-font "-xos4-terminus-medium-r-normal--14-200-100-100-c-80-iso8859-15")
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
(set-maxsize-gravity :top) ; center for floating X apps
(set-transient-gravity :top) ; center for save-as/open popups
;; (set-fg-color "#EEEEEE")
;; (set-bg-color "#1C2028")
;; (set-border-color "#232731")
;; (set-focus-color "#3B4252")
;; (set-unfocus-color "#232731")
;; (set-win-bg-color "#22272F")
;; (set-float-focus-color "#3B4252")
;; (set-float-unfocus-color "#232731")

;; Set popup window colors and font:
;;(load-module "ttf-fonts")
;; Thanks lepisma for your annotated init
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
;; (set-win-bg-color "#22272F")
;; (set-float-focus-color "#3B4252")
;; (set-float-unfocus-color "#232731")
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
;;      *mode-line-foreground-color* "#EEEEEE"
;;      *mode-line-background-color* "#1C2028"
;;      *mode-line-border-color* "#232731"
      *mode-line-border-width* 0
      *mode-line-pad-x* 0
      *mode-line-pad-y* 0
      *mode-line-position* :top)
;; (setf *bar-med-color* "^B^8")
;; (setf *bar-hi-color* "^B^3")
;; (setf *bar-crit-color* "^B^1")
;;;(setf *colors* (list "grey9"          ; 0 black
;                     "red"            ; 1 red
;                     "chartreuse3"    ; 2 green
;                     "gold1"          ; 3 yellow
;                     "steelblue3"     ; 4 blue
;                     "mediumpurple4"  ; 5 magenta
;                     "cyan3"          ; 6 cyan
;                     "honeydew4"      ; 7 white
;                     "thistle4"       ; 8 user
;                     "lightskyblue4")); 9 user
;(update-color-map (current-screen))

;(vv-set-color screen-focus-color (hex-to-xlib-color "#7780a1"))
;(vv-set-color screen-unfocus-color (hex-to-xlib-color "#101218")) 
;(vv-set-color screen-fg-color (hex-to-xlib-color "#C0C5CE"))
;(vv-set-color screen-bg-color (hex-to-xlib-color "#304050"))
;(vv-set-color screen-border-color (hex-to-xlib-color "#97BBF7"))
;(vv-set-color screen-float-focus-color (hex-to-xlib-color "#7780a1"))
;(vv-set-color screen-float-unfocus-color (hex-to-xlib-color "#101218"))
;(update-colors-all-screens)

;; Update colors
;(setf *bright-colors* nil
;      *mode-line-background-color* (hex-to-xlib-color "#111111") ; "#102030")
;      *mode-line-foreground-color* (hex-to-xlib-color "#C9CCDB")
;      *mode-line-border-color* (hex-to-xlib-color "#7780A1"))
;(mapcar #'update-color-map *screen-list*)

(update-color-map (current-screen))
