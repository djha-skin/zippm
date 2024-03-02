#+(or)
(progn
  (asdf:load-system "skin.djha.zippm")
  (asdf:test-system "skin.djha.zippm"))

(defpackage #:skin.djha.zippm/tests/resolve
  (:use #:cl)
  (:import-from
    #:skin.djha.zippm/resolve)
  (:import-from
    #:cl-semver)
  (:import-from
    #:org.shirakumo.parachute
    #:define-test
    #:true
    #:false
    #:fail
    #:is
    #:isnt
    #:is-values
    #:isnt-values
    #:of-type
    #:finish
    #:test)
  (:import-from #:esrap)
  (:local-nicknames
    (#:parachute #:org.shirakumo.parachute)
    (#:resolve #:skin.djha.zippm/resolve)))

(in-package #:skin.djha.zippm/tests/resolve)

;; NOTE: To run this test file, execute `(asdf:test-system :zippm)' in your Lisp.
+(or)
(test *)

(cl-semver:enable-version-syntax)

;; TODO: Get tests in for 

(define-test basic-structures)

(defparameter +over1+ (make-instance 'skin.djha.zippm/resolve::version-predicate
				     :relation :greater-equal
				     :version #v"1.0"))
(defparameter +under2+ (make-instance 'skin.djha.zippm/resolve::version-predicate
				      :relation :less-than
				      :version #v"2.0"))
(defparameter +at23+ (make-instance 'skin.djha.zippm/resolve::version-predicate
				    :relation :equal-to
				    :version #v"2.3"))

(defparameter +present-r+ (make-instance 'skin.djha.zippm/resolve::requirement
					 :status :present
					 :name "foo"
					 :spec
					 `(
					   (,+over1+ ,+under2+)
					   (,+at23+))))

(define-test "Make a version predicate"
	     :parent basic-structures
	     (is string= (format nil "~A" +over1+) ">=1.0.0")
	     (is string= (format nil "~A" +under2+) "<2.0.0")
	     (is string= (format nil "~A" +at23+) "==2.3.0")
	     (is string= (format nil "~A" +present-r+) "foo>=1.0.0,<2.0.0;==2.3.0"))

(define-test "Make a package information object"
	     :parent basic-structures
	     ;; TODO: Make tests use the parse stuff
	     (let ((pio
		     (resolve:make-package-info "seven-bros" (cl-semver:read-version-from-string "1.2.3") "/tmp/foo"
						(list
						  (list
						    (make-instance 'resolve::requirement
								   :status :present
								   :name "adam"
								   :spec
								   (list
								     (list
								       (make-instance 'resolve::version-predicate
										      :relation :greater-equal
										      :version (cl-semver:read-version-from-string "1.2.3"))
								       (make-instance 'resolve::version-predicate
										      :relation :less-equal
										      :version (cl-semver:read-version-from-string "1.9.7"))
								       (make-instance 'resolve::version-predicate
										      :relation :not-equal
										      :version (cl-semver:read-version-from-string "1.5.0"))
								       )
								     (list
								       (make-instance 'resolve::version-predicate
										      :relation :pess-greater
										      :version (cl-semver:read-version-from-string "3.0.0"))
								       )
								     )
								   )
						    (make-instance 'resolve::requirement
								   :status :present
								   :name "benjamin"
								   :spec
								   (list
								     (list
								       (make-instance 'resolve::version-predicate
										      :relation :equal-to
										      :version (cl-semver:read-version-from-string "89.1.0")))
								     (list
								       (make-instance 'resolve::version-predicate
										      :relation :equal-to
										      :version (cl-semver:read-version-from-string "89.5.0")))
								     (list
								       (make-instance 'resolve::version-predicate
										      :relation :equal-to
										      :version (cl-semver:read-version-from-string "94.1.0")))))
						    )
						  (list
						    (make-instance 'resolve::requirement
								   :status :absent
								   :name "caleb"
								   :spec nil)
						    (make-instance 'resolve::requirement
								   :status :present
								   :name "caleb"
								   :spec
								   (list
								     (list
								       (make-instance 'resolve::version-predicate
										      :relation :greater-equal
										      :version (cl-semver:read-version-from-string "5.0.0-alpha.3"))
								       (make-instance 'resolve::version-predicate
										      :relation :less-than
										      :version (cl-semver:read-version-from-string "5.0.0"))
								       )
								     )
								   ))
						  (list
						    (make-instance 'resolve::requirement
								   :status :present
								   :name "daniel"
								   :spec
								   nil
								   )))))
		   (pio-string "seven-bros:1.2.3@/tmp/foo(adam>=1.2.3,<=1.9.7,!=1.5.0;><3.0.0|benjamin==89.1.0;==89.5.0;==94.1.0&!caleb|caleb>=5.0.0-alpha.3,<5.0.0&daniel)"))
	       (true (string= (format nil "~A" pio)
			      pio-string))
	       (true (resolve:package-info= (esrap:parse 'resolve::package-info pio-string)
					    pio))
	       (true (string= (format nil "~A" (esrap:parse 'resolve::package-info pio-string))
			      pio-string))))

(define-test fulfillments)

(define-test "version passes"
	     :parent fulfillments
	     (let ((p (make-instance 'skin.djha.zippm/resolve::version-predicate
				     :relation :greater-equal
				     :version #v"2.0"))
		   (q (make-instance 'skin.djha.zippm/resolve::version-predicate
				     :relation :less-than
				     :version #v"3.0"))
		   (s (make-instance 'skin.djha.zippm/resolve::version-predicate
				     :relation :equal-to
				     :version #v"3.3"))
		   (u (make-instance 'skin.djha.zippm/resolve::version-predicate
				     :relation :pess-greater
				     :version #v"2.0"))
		   (w (make-instance 'skin.djha.zippm/resolve::version-predicate
				     :relation :pess-greater
				     :version #v"1.0"))
		   (v #v"2.5"))
	       (true (resolve::version-passes v p))
	       (true (resolve::version-passes v q))
	       (true (not (resolve::version-passes v s)))
	       (true (resolve::version-passes v u))
	       (true (not (resolve::version-passes v w)))))
