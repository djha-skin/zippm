(defpackage #:zippm/tests/resolve
  (:use #:cl
        #:rove)
  (:import-from 
        #:skin.djha.zippm/resolve)
  (:import-from
        #:cl-semver)
  (:import-from #:esrap)
  (:local-nicknames
        (#:resolve #:skin.djha.zippm/resolve)))

(in-package #:zippm/tests/resolve)

;; NOTE: To run this test file, execute `(asdf:test-system :zippm)' in your Lisp.
+(or)
(rove:run-test *)

(cl-semver:enable-version-syntax)

;; TODO: Get tests in for 


(deftest basic-structures
  (testing
    "Make a version predicate"
    (let ((p (make-instance 'skin.djha.zippm/resolve::version-predicate
                            :relation :greater-equal
                            :version #v"1.0"))
          (q (make-instance 'skin.djha.zippm/resolve::version-predicate
                            :relation :less-than
                            :version #v"2.0"))
          (s (make-instance 'skin.djha.zippm/resolve::version-predicate
                            :relation :equal-to
                            :version #v"2.3")))

        (ok (string=
              (format nil "~A" p)
              ">=1.0.0"))
      (let ((present-r (make-instance 'skin.djha.zippm/resolve::requirement
                              :status :present
                              :name "foo"
                              :spec
                                `(
                                  (,p ,q)
                                  (,s)))))
        (ok (string=
              (format nil "~A" present-r)
              "foo>=1.0.0,<2.0.0;==2.3.0")

              "foo>=1.0.0,<2.0.0;==2.3.0"))))
  (testing "Make a package information object"
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
             (ok (string= (format nil "~A" pio)
                          pio-string))
             (ok (resolve:package-info= (esrap:parse 'resolve::package-info pio-string)
                        pio))
             (ok (string= (format nil "~A" (esrap:parse 'resolve::package-info pio-string))
                         pio-string))

(ok (



