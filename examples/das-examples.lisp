;;; -*- mode:lisp; coding:utf-8  -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2017,2025  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/dasgen
            )     (                   2025, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //                      NOTE: This code is as ancient as mammoth shit.
             ((
              \)
|#


(das!generic msort (vector))

(das!method msort ((object vector)) (ffi:call (object "sort")))

(msort (vector 9 2 3 1 2 3 4))
;;=> #(1 2 2 3 3 4 9)

(das!method msort ((object list))
            (ffi::[]->list (msort (ffi:list->[] object))))

(msort '(1 2 3 9 8 1 2 3))
;;=> (1 1 2 2 3 3 8 9)

(das!generic msort (vector &optional function))


(das!method msort ((object vector) &optional comparator)
            (ffi:call (object "sort") comparator))

(msort (vector 9 8 7 1 2) (lambda (x y) (- x y)))
;;=> #(1 2 7 8 9)

(das!generic compare-slots (x y))

(das!method compare-slots ((x integer)(y integer))
            (cond ((< x y) 1)
                  ((= x y) 0)
                  (t -1)))

(das!method compare-slots ((x list) (y list))
            (compare-slots (length x) (length y)))

(msort (vector '(1 2 3) '(1) '(1 2) '(1 2 3)) #'compare-slots)
;;=> #((1 2 3) (1 2 3) (1 2) (1))

(time (msort (vector '(1 2 3) '(1) '(1 2) '(1 2 3)) #'compare-slots))
;;=>
;;  Execution took 0.001 seconds.
;;  #((1 2 3) (1 2 3) (1 2) (1))




;;; Example from Dmitry Ignatiev @love5an

(defstruct (negation (:constructor negation (expr)) :named (:type vector)) expr)
(def-type 'negation (lambda (p) (negation-p p)))

(defstruct (addition (:constructor addition (left right)) :named (:type vector)) left right)
(def-type 'addition (lambda (p) (addition-p p)))

(das!generic evaluate (expr))

(das!method evaluate ((expr negation))
            (- (evaluate (negation-expr expr))))

(das!method evaluate ((expr addition))
            (+ (evaluate (addition-left expr))
               (evaluate (addition-right expr))))

(das!method evaluate ((expr number))
            expr)

(evaluate (addition 5 (negation -5)))
;; ==> 10

;;; EOF
