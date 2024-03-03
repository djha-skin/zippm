#+(or)
(progn
  (asdf:load-system "skin.djha.zippm")
  (asdf:test-system "skin.djha.zippm"))

(defpackage :skin.djha.zippm/tests/resolve
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
                                         :vp-dnf
                                         `(
                                           (,+over1+ ,+under2+)
                                           (,+at23+))))
#+(or)
(test *)

(define-test "Make a version predicate"
  :parent basic-structures
  (is string= (format nil "~A" +over1+) ">=1.0.0")
  (is string= (format nil "~A" +under2+) "<2.0.0")
  (is string= (format nil "~A" +at23+) "==2.3.0")
  (is string= (format nil "~A" +present-r+) "foo>=1.0.0,<2.0.0;==2.3.0"))

(defparameter +adam-requirement+
  (make-instance
    'resolve::requirement
    :status :present
    :name "adam"
    :vp-dnf
    (list
      (list
        (make-instance
          'resolve::version-predicate
          :relation :greater-equal
          :version (cl-semver:read-version-from-string "1.2.3"))
        (make-instance
          'resolve::version-predicate
          :relation :less-equal
          :version (cl-semver:read-version-from-string "1.9.7"))
        (make-instance
          'resolve::version-predicate
          :relation :not-equal
          :version (cl-semver:read-version-from-string "1.5.0"))
        )
      (list
        (make-instance
          'resolve::version-predicate
          :relation :pess-greater
          :version (cl-semver:read-version-from-string "3.0.0"))
        )
      )))

(defparameter +benjamin-requirement+
  (make-instance
    'resolve::requirement
    :status :present
    :name "benjamin"
    :vp-dnf
    (list
      (list
        (make-instance
          'resolve::version-predicate
          :relation :equal-to
          :version
          (cl-semver:read-version-from-string
            "89.1.0")))
      (list
        (make-instance
          'resolve::version-predicate
          :relation :equal-to
          :version
          (cl-semver:read-version-from-string
            "89.5.0")))
      (list
        (make-instance
          'resolve::version-predicate
          :relation :equal-to
          :version
          (cl-semver:read-version-from-string
            "94.1.0"))))))

(defparameter
  +no-caleb-requirement+
  (make-instance
    'resolve::requirement
    :status :absent
    :name "caleb"
    :vp-dnf nil))

(defparameter
  +caleb-requirement+
  (make-instance
    'resolve::requirement
    :status :present
    :name "caleb"
    :vp-dnf
    (list
      (list
        (make-instance
          'resolve::version-predicate
          :relation :greater-equal
          :version (cl-semver:read-version-from-string
                     "5.0.0-alpha.3"))
        (make-instance
          'resolve::version-predicate
          :relation :less-than
          :version
          (cl-semver:read-version-from-string
            "5.0.0"))))))

(defparameter +daniel-requirement+
  (make-instance 'resolve::requirement
                 :status :present
                 :name "daniel"
                 :vp-dnf
                 nil))

(defparameter
  +seven-bros+
  (resolve:make-package-info
    "seven-bros"
    (cl-semver:read-version-from-string "1.2.3")
    "/tmp/foo"
    (list
      (list
        +adam-requirement+
        +benjamin-requirement+)
      (list
        +no-caleb-requirement+
        +caleb-requirement+)
      (list
        +daniel-requirement+))))

(defparameter +seven-bros-string+  "seven-bros:1.2.3@/tmp/foo(adam>=1.2.3,<=1.9.7,!=1.5.0;><3.0.0|benjamin==89.1.0;==89.5.0;==94.1.0&!caleb|caleb>=5.0.0-alpha.3,<5.0.0&daniel)")
#+(or)
(test *)
(define-test
  "Make a package information object"
  :parent basic-structures
  ;; TODO: Make tests use the parse stuff
  (is string= (format nil "~A" +seven-bros+)
      +seven-bros-string+)
  (is resolve:package-info=
      (esrap:parse 'resolve::package-info +seven-bros-string+)
      +seven-bros+)
  (is string=
      (format nil "~A"
              (esrap:parse
                'resolve::package-info
                +seven-bros-string+))
      +seven-bros-string+))

(define-test fulfillments)

(defparameter +past-2+
  (make-instance 'skin.djha.zippm/resolve::version-predicate
                 :relation :greater-equal
                 :version "2.0"))

(defparameter +under-3+
  (make-instance 'skin.djha.zippm/resolve::version-predicate
                 :relation :less-than
                 :version #v"3.0"))

(defparameter +at-3.3+
  (make-instance 'skin.djha.zippm/resolve::version-predicate
                 :relation :equal-to
                 :version #v"3.3"))

(defparameter +between-2-3+
  (make-instance 'skin.djha.zippm/resolve::version-predicate
                 :relation :pess-greater
                 :version #v"2.0"))

(defparameter +between-1-2+
  (make-instance 'skin.djha.zippm/resolve::version-predicate
                 :relation :pess-greater
                 :version #v"1.0"))

(defparameter +version-2.5+ #v"2.5")

#+(or)
(test *)

(define-test "version passes"
  :parent fulfillments
  (true (resolve::version-passes +version-2.5+ +past-2+))
  (true (resolve::version-passes +version-2.5+ +under-3+))
  (false (resolve::version-passes +version-2.5+ +at-3.3+))
  (true (resolve::version-passes +version-2.5+ +between-2-3+))
  (false (resolve::version-passes +version-2.5+ +between-1-2+)))

(define-test "matches requirement"
  :parent fulfillments
  (false (resolve::matches-requirement
           (resolve:make-package-info
             "adam"
             (cl-semver:read-version-from-string "1.2.2")
             "/tmp/adam"
             nil)
           +adam-requirement+))
  (true (resolve::matches-requirement
          (resolve:make-package-info
            "adam"
            (cl-semver:read-version-from-string "1.2.3")
            "/tmp/adam"
            nil)
          +adam-requirement+))
  (false (resolve::matches-requirement 
           (resolve:make-package-info
             "adam"
             (cl-semver:read-version-from-string "1.5.0")
             "/tmp/adam"
             nil)
           +adam-requirement+))
  (true (resolve::matches-requirement
          (resolve:make-package-info
            "adam"
            (cl-semver:read-version-from-string "1.9.7")
            "/tmp/adam"
            nil)
          +adam-requirement+))
  (false (resolve::matches-requirement
           (resolve:make-package-info
             "adam"
             (cl-semver:read-version-from-string "1.9.8")
             "/tmp/adam"
             nil)
           +adam-requirement+))
  (true (resolve::matches-requirement
          (resolve:make-package-info
            "adam"
            (cl-semver:read-version-from-string "3.0.0")
            "/tmp/adam"
            nil)
          +adam-requirement+))
  (false (resolve::matches-requirement
           (resolve:make-package-info
             "adam"
             (cl-semver:read-version-from-string "4.0.0")
             "/tmp/adam"
             nil)
           +adam-requirement+))
  (false (resolve::matches-requirement
           (resolve:make-package-info
             "caleb"
             (cl-semver:read-version-from-string "1.5.1")
             "/tmp/adam"
             nil)
           +adam-requirement+))
  (true (resolve::matches-requirement
          (resolve:make-package-info
            "caleb"
            (cl-semver:read-version-from-string "1.5.1")
            "/tmp/adam"
            nil)
          +no-caleb-requirement+)))

(cl-semver:disable-version-syntax)
