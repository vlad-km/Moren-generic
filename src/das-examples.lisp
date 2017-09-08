;;; -*- mode:lisp; coding:utf-8  -*-

;;;
;;; This file is part of the DASGEN package
;;; Copyright © 2017 Vladimir Mezentsev
;;;
;;; DASGEN - simple implementation of Generic function for JSCL.
;;;
;;; DASGEN is free software: you can redistribute it and/or modify it under
;;; the terms of the GNU General  Public License as published by the Free
;;; Software Foundation,  either version  3 of the  License, or  (at your
;;; option) any later version.
;;;
;;; DASGEN is distributed  in the hope that it will  be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should  have received a  copy of  the GNU General  Public License
;;; Version 3 from  <http://www.gnu.org/licenses/>.
;;;
;;; JSCL   - JSCL is a Common Lisp to Javascript compiler, which is bootstrapped
;;; from Common Lisp and executed from the browser.
;;; https://github.com/jscl-project/jscl
;;;



(das!generic msort (vector))
(das!method msort ((object vector))
            (funcall ((oget object "sort" "bind") object)))

(msort (vector 9 2 3 1 2 3 4))
;;=> #(1 2 2 3 3 4 9)

(das!method msort ((object list))
            (vector-to-list (msort (list-to-vector object))))

(msort '(1 2 3 9 8 1 2 3))
;;=> (1 1 2 2 3 3 8 9)

(das!generic msort (vector &optional function))


(das!method msort ((object vector) &optional comparator)
            (funcall ((oget object "sort" "bind") object comparator)))

(msort (vector 9 8 7 1 2))
;;=> #(1 2 7 8 9)

(msort (vector #\a #\b 9 8 #\c 7 1 2))
;;=> #(1 2 7 8 9 #\a #\b #\c)



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
;;  Execution took 0.014 seconds.
;;  #((1 2 3) (1 2 3) (1 2) (1))




;;;
;;; Example from Dmitry Ignatiev @love5an
;;;

(das!structure (negation (:constructor negation (expr)))
               expr)

(das/def-type 'negation
              (lambda (obj)
                  (eq (cdr (das/structure-pred obj)) 'negation )))



(das!structure (addition (:constructor addition (left right)))
               left right)

(das/def-type 'addition
              (lambda (obj)
                  (eq (cdr (das/structure-pred obj)) 'addition )))


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
