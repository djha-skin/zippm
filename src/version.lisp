;; A 
(defpackage #:skin.djha.zippm/version
  (:use :cl)
  (:import-from #:uiop)
  (:import-from #:alexandria)
  (:documentation
    "A straight implementation in common lisp of the Debian Version Comparison Algorithm."))

(in-package #:skin.djha.zippm/version)

(defparameter *nullc* (code-char 0))

(defun
    lexical-comparison (a b)
  "Lexically compare two characters according to debian version rules."
  (declare (type character a b))
  (cond ((char= a b) 0)
        ((char= a #\~) -1)
        ((char= b #\~) 1)
        ((and (alpha-char-p a)
              (not (alpha-char-p b))
              (not (char= b (code *nullc*))))
         -1)
        ((and (alpha-char-p b)
              (not (alpha-char-p a))
              (not (= a *nullc*)))
         1)
        (:else
         (- (char-code a) (char-code b)))))
;; TODOt
(defun justify-strings (a b)
  "
  Returns two seqs of equal length, composed either
  of the characters from the strings, or the null character.
  "
  (declare (type string a b))
  (let ((va (vec a))
        (vb (vec b))
        (ca (length a))
        (cb (length b)))
    (cond
      (= ca
         cb)
      [va
       vb]
      (> cb ca)
      [(into va
             (repeat (- cb ca)
                     nullc))
       vb]
      :else
      [va
       (into vb
             (repeat (- ca cb)
                     nullc))])))

