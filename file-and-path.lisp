(in-package :stumpwm)

(defun deduce-path-separator (pathstring)
  "Is this a forwardslash or backslash platform?"
  (if (find #\/ pathstring) #\/ #\\))

(defun dirname-from-pathstring (pathstring)
  (subseq pathstring 0
    (position
      (deduce-path-separator pathstring)
      pathstring :from-end t)))

(defun dirname-from-pathname (pathname)
  (dirname-from-pathstring (namestring pathname)))

(defun filename-from-pathname (pathname)
  (pathname-name (parse-namestring pathname)))

(defun filetype-from-pathstring (pathstring)
  (let ((start (position #\. pathstring :from-end t)))
    (when start
      (subseq pathstring (1+ start)))))

(defun filename-from-pathstring (pathstring)
  (let* ((separator (deduce-path-separator pathstring))
         (start (position separator pathstring :from-end t)))
    (when start
      (let ((end (position #\. pathstring :from-end t)))
        (if (and end (< end (length pathstring)) (> end start))
            (subseq pathstring (1+ start) end)
            (subseq pathstring (1+ start)))))))

(defun filetype? (path types)
  "Is the file specified in path of one of the types specified in types?"
  (member (filetype-from-pathstring path) types :test #'string=))

(defun lisp-file? (path)
  "Does path represent a lisp source file?"
  (filetype? path '("lisp" "cl" "l" "lsp")))

(defgeneric correct-path (pathname)
  (:documentation "Normalises all paths such that backslashes become forward slashes"))

(defmethod correct-path ((pathname pathname))
  (correct-path (namestring pathname)))

(defmethod correct-path ((pathname string))
  (string-replace pathname #\\ #\/))
