(in-package :stumpwm)
;; According to lepisma:
;; Inner gaps run along all the 4 borders of a frame
(setf swm-gaps:*inner-gaps-size* 12)

;; Outer gaps add more padding to the outermost borders
;; (touching the screen border)
(setf swm-gaps:*outer-gaps-size* 24)
;; end qoute

;; Remove window borders:
(setf *normal-window-border* 0)
(setf *window-border* 0)
(setf *window-border-style* :none)


;; Call toggle-gaps
(run-commands "toggle-gaps")
(define-key *root-map* (kbd "1") "toggle-gaps")

;; Set popup window colors and font:
(load-module "ttf-fonts")
(set-font (make-instance
	   'xft:font
	   :family "Fira Mono"
	   :subfamily "Bold"
	   :size 9
	   :antialias t))
;; Thanks lepisma for your annotated init
(setf *colors* (list "#3c4c55"      ; 0 black
                     "#d18ec2"      ; 1 red
                     "#ff6a6a"      ; 2 green
                     "#dada93"      ; 3 yellow
                     "#83afe5"      ; 4 blue
                     "#7fc1ca"      ; 6 cyan
                     "#9a93e1"      ; 5 magenta
                     "#899ba6"))    ; 7 white
(update-color-map (current-screen))
(set-fg-color "#899ba6")
(set-bg-color "#3c4c55")
(set-border-color "#3c4c55")


;; (set-fg-color "#eeeeee")
;; (set-bg-color "#1C2028")
;; (set-border-color "#232731")
;; (set-focus-color "#3B4252")
;; (set-unfocus-color "#232731")
;; (set-win-bg-color "#22272F")
;; (set-float-focus-color "#3B4252")
;; (set-float-unfocus-color "#232731")

;; (setf *colors* (list "#1C2028"      ; 0 black
;;                      "#BF616A"      ; 1 red
;;                      "#A3BE8C"      ; 2 green
;;                      "#EBCB8B"      ; 3 yellow
;;                      "#5E81AC"      ; 4 blue
;;                      "#B48EAD"      ; 5 magenta
;;                      "#8FBCBB"      ; 6 cyan
;;                      "#ECEFF4"))    ; 7 white

;; (update-color-map (current-screen))
