;;;; This is a package for resolving package dependencies.
;;;; In the first part we have basic plumbing for dependency resolution;
;;;; the actual dependency resolving function is at the bottom.

#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (asdf:load-system "skin.djha.zippm/resolve")
  (dolist (x '("uiop" "alexandria" "cl-semver" "esrap"))
    (asdf:load-system x)))

(defpackage #:skin.djha.zippm/resolve
  (:use :cl)
  (:import-from #:uiop)
  (:import-from #:alexandria)
  (:import-from #:cl-semver)
  (:import-from #:esrap
		#:defrule
		#:character-ranges
		#:parse
		#:?)
  (:export #:make-package-info
	   #:package-info=))

(in-package #:skin.djha.zippm/resolve)

(defparameter relation-strings
  (alexandria:alist-hash-table
    '((:greater-than . ">")
      (:greater-equal . ">=")
      (:equal-to . "==")
      (:not-equal . "!=")
      (:less-equal . "<=")
      (:less-than . "<")
      (:pess-greater . "><"))))

(deftype version-relation ()
  '(member
     :greater-than
     :greater-equal
     :equal-to
     :not-equal
     :less-equal
     :less-than
     :pess-greater)
  )

(defclass version-predicate ()
  ((relation
     :type version-relation
     :initarg :relation :reader relation)
   (version
     :type cl-semver:semantic-version
     :initarg :version :reader version))
  (:documentation "a class to represent a version predicate."))

(defmethod print-object ((obj version-predicate) strm)
  (format strm "~a"
	  (gethash (relation obj) relation-strings))
  (cl-semver:print-version (version obj) strm)
  obj)

(deftype requirement-status ()
  '(member :present :absent)
  )

(defclass package-info ()
  ((name :initarg :name
	 :initform (error "a package name is required.")
	 :type string
	 :reader name)
   (version :initarg :version
	    :type cl-semver:semantic-version
	    :initform (error "a package version is required.")
	    :reader version)
   (location :initarg :location
	     :type string
	     :initform (error "a package location is required.")
	     :reader location)
   (requirements :initarg :requirements
		 :initform nil
		 :type list ;; of lists of version requirements
		 :reader requirements))
  (:documentation "a class to represent package information."))

(defmethod print-object ((obj package-info) strm)
  (format strm "~a:"
	  (name obj))
  (cl-semver:print-version (version obj) strm)
  (format strm "@~a(~{~{~a~^|~}~^&~})"
	  (location obj)
	  (requirements obj))
  obj)

(deftype requirer ()
  '(or (member :root) package-info))

(defparameter *requirer* :root
  "
  the requirer that will be used as the default requirer for requirements.
  this field is mostly important when building the dependency graph, at the end.
  ")

(defclass requirement ()
  ((status :initarg :status
	   :type requirement-status
	   :reader status)
   (name :initarg :name
	 :type string
	 :reader name)
   (spec :initarg :spec
	 :type list
	 :reader spec)
   (requirer :initarg :requirer
	     :type requirer
	     :initform *requirer*
	     :reader requirer))
  (:documentation "a package requirement."))

(defmethod print-object ((obj requirement) strm)
  (format strm "~:[!~;~]~a~{~{~a~^,~}~^;~}"
	  (eql :present (status obj))
	  (name obj)
	  (spec obj))
  obj)

(defun decorate (requirement requirer)
  (make-instance 'requirement
		 :status (status requirement)
		 :name (name requirement)
		 :spec (spec requirement)
		 :requirer requirer))

;; todo test
(defun make-package-info (name version location requirements)
  (let* ((base-instance (make-instance 'package-info
				       :name name
				       :version version
				       :location location))
	 (*requirer* base-instance)
	 (decorated-requirements
	   (mapcar
	     (lambda (conjunction)
	       (mapcar
		 (lambda (disjunction)
		   (decorate disjunction base-instance))
		 conjunction))
	     requirements)))

    (setf (slot-value base-instance 'requirements) decorated-requirements)
    base-instance))


(defrule pess-greater "><"
	 (:constant :pess-greater))

;;;; make sure I'm in good territory
;;;; with that one roswell bug.
#+(or)
(parse 'pess-greater "><")


(defrule greater-equal ">="
	 (:constant :greater-equal))

(defrule greater-than ">"
	 (:constant :greater-than))

(defrule matches "<>"
	 (:constant :matches))

(defrule less-equal "<="
	 (:constant :less-equal))

(defrule less-than "<"
	 (:constant :less-than))

(defrule equal-to "=="
	 (:constant :equal-to))

(defrule not-equal "!="
	 (:constant :not-equal))

(defrule version-predicate
	 (and
	   (or
	     pess-greater
	     greater-equal
	     greater-than
	     matches
	     less-equal
	     less-than
	     not-equal
	     equal-to)
	   cl-semver:version)
	 (:destructure (relation version)
		       (make-instance 'version-predicate :relation relation :version version)))

(defrule vp-conjunction
	 (and
	   version-predicate
	   (*
	     (and
	       #\,
	       version-predicate)))
	 (:destructure (vp others)
		       (cons vp
			     (loop for (_ vps) in others
				   collect vps))))

(defrule vp-disjunction
	 (and
	   vp-conjunction
	   (*
	     (and
	       #\;
	       vp-conjunction)))
	 (:destructure (vpc others)
		       (cons vpc
			     (loop for (_ vpcs) in others
				   collect vpcs))))

(defrule package-name
	 (+ (not (or #\: #\@ #\( #\) #\& #\| #\! #\, #\; #\> #\< #\=)))
	 (:lambda (name)
		  (coerce name 'string)))


(defrule version-requirement
	 (and
	   (? #\!)
	   package-name
	   (? vp-disjunction))
	 (:destructure (flag name spec)
		       (make-instance
			 'requirement
			 :status
			 (if flag :absent :present)
			 :name (coerce name 'string)
			 :spec spec)))

(defrule vr-conjunction
	 (and
	   vr-disjunction
	   (*
	     (and
	       #\&
	       vr-disjunction)))
	 (:destructure (vrc others)
		       (cons vrc
			     (loop for (_ vrcs) in others
				   collect vrcs))))

(defrule vr-disjunction
	 (and
	   version-requirement
	   (*
	     (and
	       #\|
	       version-requirement)))
	 (:destructure (vr others)
		       (cons vr
			     (loop for (_ vrs) in others
				   collect vrs))))

(defrule package-info
	 (and
	   package-name
	   #\:
	   cl-semver:version
	   #\@
	   (+ (not #\())
	   #\(
	   (? vr-conjunction)
	   #\)
	   )
	 (:destructure (name colon version at location open-paren requirements close-paren)
		       (declare (ignore colon at open-paren close-paren))
		       (make-package-info name version (coerce location 'string) requirements)))

(defun version-predicate= (a b)
  (declare (type version-predicate a b))
  (and
    (eql (relation a) (relation b))
    (cl-semver:version= (version a) (version b))))


;; this does not test requirer equality, as that would cause an infinite loop,
;; since it represents a cycle in the graph.
(defun requirement= (a b)
  (declare (type requirement a b))
  (and
    (eql (status a) (status b))
    (string= (name a) (name b))
    (every (lambda (x y)
	     (every (lambda (x y)
		      (version-predicate= x y))
		    x
		    y))
	   (spec a)
	   (spec b))))

(defun package-info= (a b)
  (declare (type package-info a b))
  (and
    (string= (name a) (name b))
    (cl-semver:version= (version a) (version b))
    (string= (location a) (location b))
    (every (lambda (x y)
	     (every (lambda (u v)
		      (and (requirement= u v)
			   (eq (requirer u) a)
			   (eq (requirer v) b)))
		    x
		    y))
	   (requirements a)
	   (requirements b))))

#+(or)
(progn

  (multiple-value-bind (production position succeeded)
    (parse 'version-predicate ">=1.2.3")
    (list production position succeeded))
  (+ (parse 'version-predicate ">=1.2.3") 3)
  (parse 'vp-disjunction ">=1.2.3,<=2.0.0,>=1.5.0;><3.0.0,!=3.2.3")
  (requirer (parse 'version-requirement "!foo>=1.2.3,<=2.0.0,=>1.5.0;><3.0.0,!=3.2.3"))
  (parse 'version-requirement "foo>=1.2.3,<=2.0.0,>1.5.0;><3.0.0,!=3.2.3")
  )

#+(or)
;; => seven-bros:1.2.3@/tmp/foo(adam>=1.2.3,<=1.9.7,!=1.5.0;><3.0.0|benjamin==89.1.0;==89.5.0;==94.1.0&!caleb|caleb>=5.0.0-alpha.3,<5.0.0&daniel)

;; This was for an older version that used parse, but we're going to use parse
;; elsewhere instead.
#+(or)
(make-instance 'package-info
	       :name "foo"
	       :version (cl-semver:read-version-from-string "1.2.3")
	       :location "/tmp/foo"
	       :requirements
	       (list
		 (list
		   (parse 'version-requirement "bar>=1.2.3,<=2.0.0,>=1.5.0;><3.0.0,!=3.2.3")
		   (parse 'version-requirement "il>=1.2.3,<=2.0.0,=>1.5.0;><3.0.0,!=3.2.3"))
		 (list
		   (parse 'version-requirement "for>=1.2.3,<=2.0.0,=>1.5.0;><3.0.0,!=3.2.3")
		   (parse 'version-requirement "baz>=1.2.3,<=2.0.0,=>1.5.0;><3.0.0,!=3.2.3"))))

(defun present (name &key spec)
  (make-instance 'requirement :status :present :name name :spec spec))

(defun absent (name &key spec)
  (make-instance 'requirement :status :absent :name name :spec spec))


(defun version-passes (ver pred)
  (declare (type cl-semver:semantic-version version)
	   (type version-predicate pred))
  (case (relation pred)
    (:greater-than (cl-semver:version> ver (version pred)))
    (:greater-equal (cl-semver:version>= ver (version pred)))
    (:equal-to (cl-semver:version= ver (version pred)))
    (:not-equal (cl-semver:version/= ver (version pred)))
    (:less-equal (cl-semver:version<= ver (version pred)))
    (:less-than (cl-semver:version< ver (version pred)))
    (:pess-greater (and
		     (cl-semver:version>= ver (version pred))
		     (cl-semver:version<
		       ver
		       (cl-semver:make-semantic-version
			 (1+ (cl-semver:version-major (version pred))) 0 0))))))

;(defun requirement-fulfills (req pkg)
;  (declare (type requirement req)
;           (type package-info pkg))
;  (and
;    (eql (status requirement) :present)
;    (string= (name requirement) (name package))
;    (every (lambda (spec)
;             (some (lambda (predicate)
;                     (cl-semver:satisfies? (version package) predicate))
;                   spec))
;           (spec requirement))))
;
;  (with-slots (name version requirements) package
;    (if (string= (name requirement) name)
;      (every (lambda (spec)
;               (some (lambda (predicate)
;                       (cl-semver:satisfies? version predicate))
;                     spec))
;             (spec requirement))
;      nil)))
;  (let ((name (name package))
;        (version (version package))
;        (requirements (requirements package)))
;    (if (string= (name requirement) name)
;      (every (lambda (spec)
;               (some (lambda (predicate)
;                       (cl-semver:satisfies? version predicate))
;                     spec))
;             (spec requirement))
;      nil)))
;
;
;
;
;(defgeneric fulfills (requirement package)
;  (:documentation "A generic function to determine if a package fulfills a requirement."))
