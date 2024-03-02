(defpackage :skin.djha.zippm/tests
  (:use #:cl)
  (:import-from
    #:skin.djha.zippm)
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
    (#:zippm #:skin.djha.zippm)))

(in-package #:skin.djha.zippm/tests)

(define-test main)
