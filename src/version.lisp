;; A
(defpackage #:skin.djha.zippm/version
  (:use :cl)
  (:import-from #:uiop)
  (:import-from #:alexandria)
  (:documentation
    "A straight implementation in common lisp of the Debian Version Comparison Algorithm."))

(in-package #:skin.djha.zippm/version)

(defun justify
    (seq len &key make-array-args)
    "Return a vector of the given length, with the given sequence justified to
     the left."
  (let ((justified (apply #'make-array
                          (concatenate 'list
                                       (list len)
                                       make-array-args))))
    (replace justified seq :start1 0 :end1 (min len
                                                (length seq)))
    justified))

(defun rectify
    (a b &rest make-array-args)
  "Return two sequences of the same length, with the shorter one justified to
  the left."
  (let ((la (length a))
        (lb (length b))
        (maxlength (max la lb))
        (ja (if (= maxlength la) a (justify a
                                            maxlength
                                            :make-array-args
                                            make-array-args)))
        (jb (if (= maxlength lb) b (justify b maxlength
                                            :make-array-args
                                            make-array-args))))
    (values maxlength ja jb)))

(defparameter *nullc* (code-char 0))

(defun nonnumeric-part-compare
    (a b)
  (if (string= a b)
      0
      (multiple-value-bind
          (maxlength ja jb)
          (rectify a b
                   :element-type 'char
                   :initial-element *nullc*)
        (loop for i from 0 below maxlength
              for a = (elt ja i)
              for b = (elt jb i)
              do
              (let ((diff
                      (cond ((char= a b) 0)
                            ((char= a #\~) -1)
                            ((char= b #\~) 1)
                            ((char= a *nullc*) -1)
                            ((char= b *nullc*) 1)
                            ((and (alpha-char-p a)
                                  (not (alpha-char-p b))) -1)
                            ((and (alpha-char-p b)
                                  (not (alpha-char-p a))) 1)
                            (:else
                             (- (char-code a) (char-code b))))))
                (when (not (zerop diff))
                  (return diff)))
              finally (return 0)))))

(defun numeric-part-compare
  (a b)
  "Compares two version parts, which both consist
  entirely of digits."
  (declare (type string a b))
  (let ((trimmed-a (string-left-trim '(#\0) a))
        (trimmed-b (string-left-trim '(#\0) b))
        (ldiff (- (length trimmed-a)
                  (length trimmed-b))))
    (if (zerop ldiff)
        (cond ((string= trimmed-a trimmed-b) 0)
              ((string< trimmed-a trimmed-b) -1)
              :else 1)
        ldiff)))

(defun version-part-compare (i a b)
  "
  Compare the `i`th part of the parts of version strings `a` and `b`, as found
  by `version-parts-split`.
  "
  (declare (type fixnum i)
           (type string a b))
  (let ((comparator (if (zerop (mod i 2))
                        #'nonnumeric-part-compare
                        #'numeric-part-compare)))
    (funcall comparator a b)))

(defun version-parts-split
    (version)
  "
  Split a version string up into its component parts, with digits in the `0,2,4,...`th
  spots and non-digits in the `1,3,5,...`th spots.
  "
  (declare (type string version))
  (loop
    for i = 0 then (1+ i)
    for check = (if (zerop (mod i 2))
                    #'digit-char-p
                    (complement #'digit-char-p))
    for scratch = version then (subseq scratch next-checked)
    for next-checked = (or
                         (position-if check scratch)
                         (length scratch))
    while (not (zerop (length scratch)))
    collect (subseq scratch 0 next-checked)))

(defun epochless-vercmp (a b)
  "
  Compare two version numbers, assuming that neither has an epoch.
  "
  (declare (type string a b))
  (if (equal a b)
      0
      (let ((split-a (version-parts-split a))
            (split-b (version-parts-split b)))
        (multiple-value-bind
            (maxlength ja jb)
            (rectify split-a split-b
                     :element-type 'string
                     :initial-element "")
          (loop for i = 0 then (1+ i)
                for a across ja
                for b across jb
                for cmp = (version-part-compare i a b)
                while (zerop cmp)
                finally (return cmp))))))

(defun epoch
    (a)
  "
  Extract the epoch from a debian version string.
  "
  (let ((found (position #\: a)))
    (if found
        (values (parse-integer (subseq a 0 found))
                (subseq a (1+ found)))
        (values 0 a))))

(defun vercmp
    (a b)
  "
  Compares two version numbers according to the rules laid out in the [Debian
  Policy
  Manual](https://www.debian.org/doc/gpolicy/ch-controlfields.html#s-f-Version),
  retrieved 2024-02-15.

  Epoch numbers, upstream versions, and revision version parts are fully
  supported.
  "
  (multiple-value-bind (a-epoch a-vers) (epoch a)
    (multiple-value-bind (b-epoch b-vers) (epoch b)
      (if (= a-epoch b-epoch)
          (epochless-gvercmp a-vers b-vers)
          (- a-epoch b-epoch)))))

;; TODO TEST
