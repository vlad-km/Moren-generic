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

;;;
;;; DAS!STRUCTURE
;;; Some modify from original file jscl-project/jscl/src/defstruct.lisp
;;;
;;; Release: Not canonical, simple, without MOP
;;;          Built on storage-vector
;;;          Small modify original def!struct
;;;
;;; Additionally implemented options:
;;;       :conc-name
;;;       :contsructor (with &key and &optional form)
;;;
;;; Not supported:
;;;       :include option
;;;       :type    option
;;; and other cookies from the canon, because without MOP
;;;
;;; Purpose:
;;;
;;;     Acceleration of work compared to implementation def!struct on lists
;;;     Use as part of the DASGEN package and separately, so.
;;;     Does not conflict with the existing implementation of the def!struct
;;;
;;;
;;; Documentation: see below
;;;
;;;




;;;
;;; todo: Remove if compile at host jscl environment
;;;
(defmacro with-collect (&body body)
    "Makes available to BODY a function named collect. The function accumulates
values passed to it. The return value of with-collect is the list of values
accumulated, in the order."
    (let ((head (gensym))
          (tail (gensym)))
        `(let* ((,head (cons 'sentinel nil))
                (,tail ,head))
             (flet ((collect (x)
                        (rplacd ,tail (cons x nil))
                        (setq ,tail (cdr ,tail))
                        x))
                 ,@body)
             (cdr ,head))))

;;;
;;; (das!structure nameop field .... field)
;;;
;;;  nameop = name | (name option ... option)
;;;  option = constructor | (:predicate name) | (:copy name) | (:conc-name name)
;;;  constructor = (:constructor name) | (:constructor name (arg ... arg))
;;;  filed = var | (var initform)
;;;  initform = any legal expression
;;;  var = symbol name
;;;
;;; (das!structure negation expr)
;;;    contructor form:
;;;               (defun make-negation (&key expr))
;;;               (make-negation) (make-negation :expr 123)
;;;
;;; (das!structure (negation (:constructor negation (expr))) expr)
;;;    constructor form:
;;;               (defun negation (&optional expr))
;;;               (negation)        ;=> slot expr is nil
;;;               (negation 123)    ;=> slot expr is 123
;;;
;;; (das!structure (negation (:constructor negation (expr ))) (expr 123))
;;;               (defun negation (&optional (expr 123)))
;;;
;;; ie &key vs &optional form
;;;
;;; Full form:
;;;
;;; (das!structure kik count) | (das!structure kik (count 99))
;;;    (make-kik) | (make-kik :count 1)
;;;
;;; (das!structure (kik (:constructor kik) (:conc-name what-)) count)
;;;    (kik) | (kik :count 22)   - contructor
;;;    (what-count *p)           - accessor
;;;    (kik-p *p)                - predicat
;;;    (copy-kik *p)             - copier
;;;
;;; (das!structure (kik (:constructor kik (count)) (:conc-name what-)) (count 99))
;;;    (kik) | (kik 33)
;;;    (what-count *p)
;;;    (kik-p *p)
;;;    (copy-kik *p)
;;;
;;;
;;; DAS/TYPE-DEF kids
;;;
;;; (cons das . ,name) where name its structure name
;;;
;;; If you want to include a structure in the das/type hierarchy, use the call
;;;            (das/def-type name predicat)
;;;
;;; something like:
;;;   (def!structure ship name tonnage)
;;;   (das/def-type 'ship #'ship-p)
;;;   (das/type-of (make-ship) 'ship)
;;;   => t
;;;   (das/typep (make-ship) 'ship)
;;;   => t
;;;


(export '(das/structure-pred))



;;; true if x is structure
;;; (cons structure name)
(defun das/structure-pred (x)
    (if (storage-vector-p x)
        (let ((kids (storage-vector-ref x 0)))
            (if (eq (car kids) 'structure)
                kids))))

(defun das/structure-type-kid (x)
    (cdr (storage-vector-ref x 0)))



(defmacro das!structure (name-and-options &rest slots)
    (let* ((name-and-options (ensure-list name-and-options))
           (name (first name-and-options))
           (name-string (symbol-name name))
           (options (rest name-and-options))
           (type-definition)
           (lform)
           (conc-name
             (let* ((form (assoc :conc-name options))
                    (concname (if form
                                  (if (second form)
                                      (symbol-name (second form))
                                      (concat name-string "-") )
                                  (concat name-string "-"))))
                 concname))
           (constructor (assoc :constructor options))
           (constructor-form (lambda ()
                                 (if constructor
                                     (values (second constructor) (third constructor))
                                     (values nil nil))) )
           (predicate (if (assoc :predicate options)
                          (second (assoc :predicate options))
                          (intern (concat name-string "-P"))))
           (copier (if (assoc :copier options)
                       (second (assoc :copier options))
                       (intern (concat "COPY-" name-string)))))
        (multiple-value-bind (construct-name args) (funcall constructor-form)
            (if construct-name
                (setq constructor construct-name)
                (setq constructor (intern (concat "MAKE-" name-string))))
            (if args
                (setq lform '&optional)
                (setq lform '&key)))
        (let* ((slot-descriptions
                 (mapcar (lambda (sd)
                             (cond
                               ((symbolp sd)
                                (list sd))
                               ((and (listp sd) (car sd) (null (cddr sd)))
                                sd)
                               (t
                                (error "Bad slot description `~S'." sd))))
                         slots))
               constructor-expansion
               predicate-expansion
               copier-expansion)
            (when constructor
                (setq constructor-expansion
                      `(defun ,constructor (,lform ,@slot-descriptions)
                           (vector (cons 'structure ',name) ,@(mapcar #'car slot-descriptions)))))
            (when predicate
                (setq predicate-expansion
                      `(defun ,predicate (x)
                           (eq (cdr (das/structure-pred x)) ',name))))
            (when copier
                (setq copier-expansion
                      `(defun ,copier (x)
                           ;;(unless (,predicate x)
                           ;;    (error "The object `~S' is not of type `~S'" x ,name-string))
                           ;; copy array
                           ;; lazy version - used list-to-vector / vector-to-list / copy-list
                           ;; todo: performance test
                           ;;
                           (let ((copy (list-to-vector
                                        (copy-list (vector-to-list x)))))
                               copy))))
            `(progn
                 ,constructor-expansion
                 ,predicate-expansion
                 ,copier-expansion
                 ,type-definition
                 ;; Slot accessors
                 ,@(with-collect
                       (let ((index 1))
                           (dolist (slot slot-descriptions)
                               (let* ((name (car slot))
                                      (accessor-name (intern (concat conc-name (string name))))
                                      (setter-name (intern (concat "SET-" conc-name (string name)))))
                                   (collect
                                       `(defun ,setter-name (x value)
                                            ;;(unless (,predicate x)
                                            ;;    (error "The object `~S' is not of type `~S'" x ,name-string))
                                            (storage-vector-set x ,index value)))


                                   (collect
                                       `(defun ,accessor-name (x)
                                            ;;(unless (,predicate x)
                                            ;;    (error "The object `~S' is not of type `~S'" x ,name-string))
                                            (storage-vector-ref x ,index)))
                                   (collect
                                       `(define-setf-expander ,accessor-name (x)
                                            (let ((object (gensym))
                                                  (new-value (gensym)))
                                                (values (list object)
                                                        (list x)
                                                        (list new-value)
                                                        ;;`(storage-vector-set ,object ,',index ,new-value)
                                                        ;;`(,',accessor-name ,object)
                                                        `(storage-vector-set ,object ,',index ,new-value)
                                                        `(storage-vector-ref ,object ,',index)))))
                                   (setq index (1+ index))   ))))

                 ',name) )))


;;; EOF
