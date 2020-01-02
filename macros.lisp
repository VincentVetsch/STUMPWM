
(defmacro while (predicate &body body)
  "Create a standard while loop"
  `(loop :while ,predicate
	:do ,@body))

(defmacro defprogram (name command key properties &key rat)
  "macro that sets up a program like use-package in emacs"
  (let ((name-focus (intern1 (concat (string name) "-FOCUS")))
	(name-pull (intern1 (concat "PULL-" (string name)))))
    `(progn
       (shifting-command ,name ,properties ,command ,rat)
       (define-key *top-map*
	   (kbd ,(concatenate 'string "s-" key))
	 ,(string name))
       (defcommand ,name-focus () ()
	 "run this command"
	 (run-shell-command ,command))
       (define-key *top-map*
	   (kbd ,(concatenate 'string "s-" (string-upcase key)))
	 ,(string name-focus))
       (defcommand ,name-pull () ()
	 "pull this command"
	 (run-or-pull ,command ,properties))
       (define-key *root-map*
	   (kbd ,(concatenate 'string "C-" (string-upcase key)))
,(string name-pull)))))

(defmacro vv-set-color (val color)
  "Similar to `set-any-color', but without updating colors."
  `(dolist (s *screen-list*)
     (setf (,val s) (alloc-color s ,color))))

(defmacro with-new-window ((window cmd &key properties (timeout 30))
                           &body body)
  "Execute command, on next new window matching properties, run the body.  If no
properties given, next new window will be acted on.
By default, code will run in *focus-window-hook* handler, but can also run in
*new-window-hook* handler by using keyword :new.  Return to focus hook with
:focus."
  (let ((state 'config)
        (init '())
        (config '()))
    (map nil #'(lambda (e)
                 (case e
                   (:focus (setf state 'config))
                   (:new (setf state 'init))
                   (t (ecase state
                        (init (push e init))
                        (config (push e config))))))
         body)
    `(run-and-act-on-new-window ,cmd ,properties ,timeout
                                #'(lambda (,window)
                                    ,@(reverse init))
                                #'(lambda (,window)
,@(reverse config)))))
