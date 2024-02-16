(defpackage #:zippm/tests/version
  (:use #:cl
        #:skin.djha.zippm/version
        #:rove))
(in-package #:zippm/tests/version)

;; NOTE: To run this test file, execute `(asdf:test-system :zippm)' in your Lisp.
+(or)
(rove:run-test *)

(deftest version-parts-split
  (testing
    "The empty case. This case doesn't really make sense, but it's here for
    completeness."
    (ok (equal (skin.djha.zippm/version::version-parts-split "") nil)))
  (testing
    "Some simple cases"
    (ok (equal (skin.djha.zippm/version::version-parts-split " ") '(" ")))
    (ok (equal (skin.djha.zippm/version::version-parts-split "5") '("" "5")))
    (ok (equal (skin.djha.zippm/version::version-parts-split "1.0")
               '("" "1" "." "0")))
    (ok (equal (skin.djha.zippm/version::version-parts-split "1.0.0")
               '("" "1" "." "0" "." "0"))))
  (testing
    "A case with a numeric suffix"
    (ok (equal (skin.djha.zippm/version::version-parts-split "5.3.alpha17")
        '("" "5" "." "3" ".alpha" "17"))))
  (testing
    "Concerning dots"
    (ok (equal (skin.djha.zippm/version::version-parts-split ".a1")
        '(".a" "1")))
    (ok (equal (skin.djha.zippm/version::version-parts-split "5.a1")
        '("" "5" ".a" "1"))))
  (testing
    "A case with a tilde"
    (ok (equal (skin.djha.zippm/version::version-parts-split "1.0~alpha")
               '("" "1" "." "0" "~alpha")))))
