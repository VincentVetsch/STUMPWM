(in-package :stumpwm)

(mapcar #'load-module '(
			"cpu"
			"disk"
			"hostname"
			"mem"
			"net"
			))

(setf *screen-mode-line-format*
      (list
       " ^4%g " ;Group name
       ;"^B^2(%n (^4%g^B^2)) ^b" ;Group name
       " ^2%v "  ;Window title
       " ^> "  ;align right
;;       "^n^03 %c"  ;cpu load
       "^n %c"  ;cpu load
       "%f "  ;cpu speed
       "%t "  ;cpu temp
       ; "%M "   ;memory
       ;" %D "  ;disk
       " %l "  ;Internet
       "^02 %d "
       ))
(setf *mode-line-timeout* 1)

;1 is red, 2 is green, 3 is yellow, 4 is blue, 5 is majenta, 6 is cyan, 7 is white, 
