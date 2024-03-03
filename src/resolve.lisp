;;;; This is a package for resolving package dependencies.
;;;; In the first part we have basic plumbing for dependency resolution;
;;;; the actual dependency resolving function is at the bottom.

#+(or)
(progn
  (declaim (optimize (speed 0) (space 0) (debug 3)))
  (asdf:load-system "skin.djha.zippm")
  (asdf:test-system "skin.djha.zippm")
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
   (req-cnf :initarg :req-cnf
            :initform nil
            :type list                     ; of lists of requirements
            :reader req-cnf))
  (:documentation "a class to represent package information."))

(defmethod print-object ((obj package-info) strm)
  (format strm "~a:"
          (name obj))
  (cl-semver:print-version (version obj) strm)
  (format strm "@~a(~{~{~a~^|~}~^&~})"
          (location obj)
          (req-cnf obj))
  obj)

(deftype requirer ()
  '(or (member :root) package-info))

(defparameter *requirer* :root
  "
  the requirer that will be used as the default requirer for req-cnf.
  this field is mostly important when building the dependency graph, at the end.
  ")

(defclass requirement ()
  ((status :initarg :status
           :type requirement-status
           :reader status)
   (name :initarg :name
         :type string
         :reader name)
   ;; This is a DNF of version-predicates.
   (vp-dnf :initarg :vp-dnf
           :type list
           :reader vp-dnf)
   (requirer :initarg :requirer
             :type requirer
             :initform *requirer*
             :reader requirer))
  (:documentation "a package requirement."))

(defmethod print-object ((obj requirement) strm)
  (format strm "~:[!~;~]~A~{~{~A~^,~}~^;~}"
          (eql :present (status obj))
          (name obj)
          (vp-dnf obj))
  obj)

(defun decorate (requirement requirer)
  (make-instance 'requirement
                 :status (status requirement)
                 :name (name requirement)
                 :vp-dnf (vp-dnf requirement)
                 :requirer requirer))

(defun make-package-info (name version location req-cnf)
  (let* ((base-instance (make-instance 'package-info
                                       :name name
                                       :version version
                                       :location location))
         (*requirer* base-instance)
         (decorated-req-cnf
           (mapcar
             (lambda (conjunction)
               (mapcar
                 (lambda (disjunction)
                   (decorate disjunction base-instance))
                 conjunction))
             req-cnf)))

    (setf (slot-value base-instance 'req-cnf) decorated-req-cnf)
    base-instance))


(defrule pess-greater "><"
  (:constant :pess-greater))

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

(defrule requirement
  (and
    (? #\!)
    package-name
    (? vp-disjunction))
  (:destructure (flag name vp-dnf)
   (make-instance
     'requirement
     :status
     (if flag :absent :present)
     :name (coerce name 'string)
     :vp-dnf vp-dnf)))

(defrule req-conjunction
  (and
    req-disjunction
    (*
      (and
        #\&
        req-disjunction)))
  (:destructure (reqc others)
   (cons reqc
         (loop for (_ reqcs) in others
               collect reqcs))))

(defrule req-disjunction
  (and
    requirement
    (*
      (and
        #\|
        requirement)))
  (:destructure (req others)
   (cons req
         (loop for (_ reqs) in others
               collect reqs))))

(defrule package-info
  (and
    package-name
    #\:
    cl-semver:version
    #\@
    (+ (not #\())
    #\(
    (? req-conjunction)
    #\)
    )
  (:destructure (name colon version at location open-paren req-cnf close-paren)
   (declare (ignore colon at open-paren close-paren))
   (make-package-info name version (coerce location 'string) req-cnf)))

(defun version-predicate= (a b)
  (declare (type version-predicate a b))
  (and
    (eql (relation a) (relation b))
    (cl-semver:version= (version a) (version b))))


;;; This does not test requirer equality, as that would cause an infinite loop,
;;; since it represents a cycle in the graph.
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
           (vp-dnf a)
           (vp-dnf b))))

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
           (req-cnf a)
           (req-cnf b))))

(defun present (name &key vp-dnf)
  (make-instance 'requirement :status :present :name name :vp-dnf vp-dnf))

(defun absent (name &key vp-dnf)
  (make-instance 'requirement :status :absent :name name :vp-dnf vp-dnf))

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

(defun matches-requirement (pkg req)
  (declare (type package-info pkg)
           (type requirement req))
  (and
    (string= (name req) (name pkg))
    (if (null (vp-dnf req))
        t
        (some (lambda (vp-dnf-clause)
                (every (lambda (predicate)
                         (version-passes (version pkg) predicate))
                       vp-dnf-clause))
              (vp-dnf req)))))

