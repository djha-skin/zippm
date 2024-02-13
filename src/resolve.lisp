(defpackage #:skin.djha.zippm/resolve
  (:use :cl)
  (:import-from #:uiop)
  (:import-from #:alexandria)
  )

(in-package #:skin.djha.zippm/resolve)

(defparameter relation-strings
  (alexandria:alist-hash-table
    '((:greater-than . ">")
      (:greater-equal . ">=")
      (:equal-to . "==")
      (:not-equal . "!=")
      (:less-equal . "<=")
      (:less-than . "<")
      (:matches . "<>")
      (:in-range . "=>")
      (:pess-greater . "><"))))

(defclass version-predicate ()
  ((relation :initarg :relation :reader relation)
   (version :initarg :version :reader version))
  (:documentation "A class to represent a version predicate."))

(defmethod print-object ((obj version-predicate) strm)
  (format strm "~A~A"
          (gethash (relation obj) relation-strings)
          (version obj)))

(defclass requirement ()
  ((status :initarg :status :reader status)
   (name :initarg :name :reader name)
   (spec :initarg :spec :reader spec))
  (:documentation "A package requirement."))

(defmethod print-object ((obj requirement) strm)
    (format strm "~:[!~;~]~A~{~{~A~^,~}~^;~}"
            (eql :present (status obj))
            (name obj)
            (spec obj)))

(defclass package-info
  ((name :initarg :name :reader name)
   (version :initarg :version :reader version)
   (location :initarg :location :reader location)
   (requirements :initarg :requirements :reader requirements))
  (:documentation "A class to represent package information."))

(defun present (name &key spec)
  (make-instance 'requirement :status :present :name name :spec spec))

(defun absent (name &key spec)
  (make-instance 'requirement :status :absent :name name :spec spec))

(defun spec-holds (cmp spec package)
  (declare (type (or null version-predicate) spec)
           (type package-info package))
  (if (null spec)
      t
      (let ((pkg-version (version package)))
        (reduce (lambda (disj-acc disj-req)
                  (or disj-acc
                      (reduce
                        (lambda (conj-acc conj-req)
                          (make-comparison


