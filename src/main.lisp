#+(or)
(asdf:load-system "skin.djha.zippm")

(defpackage #:skin.djha.zippm
  (:use :cl)
  (:import-from #:uiop)
  (:import-from #:alexandria)
  (:import-from #:cl-i)
  (:export
    entrypoint))

(in-package #:skin.djha.zippm)

(defun stub (options name)
  (let ((strm (gethash :strm options)))
    (format strm "~A~%" name)
    (loop for k being the hash-keys of options
	  using (hash-value v)
	  do (format strm "~39@A: ~A~%" k v)
	  finally (return (alexandria:alist-hash-table
			    `((:status . :successful)
			      (:options . ,options))
			    :test #'equal)))))

(defun athing (options)
  (declare (type hash-table options))
  (stub options "athing"))

(defun main (argv &key (strm t))
  "
  The functional entrypoint of the zippm command.
  "
  (declare (type list argv))
  (cl-i:execute-program
    "zippm"
    (cl-i:system-environment-variables)
    `(
      (("athing") . ,#'athing)
      )
    :helps
    `((("athing") . "A stub function"))
    :cli-arguments argv
    :setup (lambda (opts)
	     (setf (gethash :strm opts) strm)
	     opts)
    :teardown (lambda (result)
		(format strm "~A~%"
			(cl-i:generate-string result :pretty 4)
			)
		result)))

(defun entrypoint (argv)
  (uiop:quit
    (main argv :strm *standard-output*)))
