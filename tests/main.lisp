(defpackage #:zippm/tests/resolve
  (:use #:cl
        #:skin.djha.zippm/resolve
        #:rove))
(in-package #:zippm/tests/resolve)

;; NOTE: To run this test file, execute `(asdf:test-system :zippm)' in your Lisp.
+(or)
(rove:run-test *)

(deftest basic-structures
  (testing
    "Make a version predicate"
    (let ((p (make-instance 'skin.djha.zippm/resolve::version-predicate
                            :relation :greater-equal
                            :version "1.0"))
          (q (make-instance 'skin.djha.zippm/resolve::version-predicate
                            :relation :less-than
                            :version "2.0"))
          (s (make-instance 'skin.djha.zippm/resolve::version-predicate
                            :relation :equal-to
                            :version "2.3")))

        (ok (string=
              (format nil "~A" p)
              ">=1.0"))
      (let ((present-r (make-instance 'skin.djha.zippm/resolve::requirement
                              :status :present
                              :name "foo"
                              :spec
                                `(
                                  (,p ,q)
                                  (,s)))))
        (ok (string=
              (format nil "~A" present-r)
              "foo>=1.0,<2.0;==2.3")

              "foo>=1.0,<2.0;==2.3")))))
