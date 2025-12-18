;;; -*- mode:lisp; coding:utf-8  -*-

#|

            /\___/\
            )     (
           =\     /=                  
             )   (                    Copyright © 2017,2025  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/dasgen
            )     (                   2025, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  Moren environment
      jgs   \__ __/
               ))
              //                      
             ((
              \)
|#


#|            Performanse note
              preliminarily

(das!generic tik (t t t))
(das!method tik (a b c))
(das!method tik ((a list) x y) (values :a-list))

(time (dotimes (i 100000) (tik '(1) 2 3)))
   Execution took 1.333 seconds.
   Execution took 1.342 seconds.
   Execution took 1.325 seconds.

(defgeneric tak (t t t))
(defmethod tak (a b c) :gen)
(defmethod tak ((a list) b c) :list)

(time (dotimes (i 100000) (tak '(1) 2 3)))
   Execution took 5.329 seconds.

|#

(in-package :das)

;;; TYpe definition structure
(defstruct (das-typedef (:type vector) :named) type supertype predicate class)

;;; das types datum
(defvar *das-types* nil)
(setq *das-types* (make-hash-table :test #'equal))

;;; errors
(defvar *dasgen-typer*)
(setq *dasgen-typer*
      #(
        "DAS: wrong `def-type` form (~a ~a).~%Expected two arguments minimum." ;; def-type
        "DAS: type-def ~a not exists."                                         ;; das/find-typedef
        "DAS: wtf a  ~a ?"                                                     ;; the-type-of
        "DAS: wrong type-def :predicate for ~a, `function` expected."          ;; the-typep
        "DAS: invalid type ~a, `symbol` expected"                              ;; 
        ))

(defconstant +wrong-deftype-form+ 0)  ;; def-type
(defconstant +typedef-not-exists+ 1) ;; find-typedef
(defconstant +wtf+ 2) ;; the-type-of
(defconstant +function-expected+ 3) ;; the-typep
(defconstant +symbol-expected+ 4)

(defun das/typer-raise (n-error &rest arguments)
  (apply 'error (push (aref *dasgen-typer* n-error) arguments)))

;;; definition of the generic type
;;;
;;; - any structures. structure must be defined as :named. i.e. (defstruct (ship (:type vector) :named) name deadweight))
;;; - any entities, why there have own's predicate
;;;      (def-type 'name (lambda (x) (structure-p x)))
;;;      or
;;;      (def-type 'name 'predicate-fn)
(export '(das::def-type))
(defun def-type (&key (type) (predicate nil predicate-p) (supertype nil super-p))
  (when (or (null type) (null predicate-p))
    (das/typer-raise +wrong-deftype-form+  type predicate))
  (let ()
    (check-type type symbol)
    (check-type predicate function)
    (when supertype (check-type supertype symbol))
    (setf (gethash (car typedef) *das-types*)
          (make-das-typedef :type type
                            :predicate predicate
                            :supertype supertype
                            :class class))))

;;; Find deftype for symbol type
(defun das/find-typedef (type)
  (let ((ok (gethash type *das-types*)))
    (if ok ok (das/typer-raise +typedef-not-exists+ type))))

;;; NOTE: WHAT IS ?
(defun das/standard-object-type-kid (obj)
  (storage-vector-ref obj 0))

;;; todo: fix
;;; types/classes hierarchy
;;;
;;; t             atom
;;; character      t
;;; function       t
;;; array          t
;;; number         t
;;; float          number
;;; integer        number
;;; symbol         t
;;; vector         array
;;; string         vector
;;; list           sequence
;;; cons           list
;;; null           list
;;; hash-table     t
;;;

;;; tiny base-types
(let ()
  (defparameter *das-basic-types*
    '((hash-table hash-table-p t)
      (number numberp t)
      (integer integerp number)
      (float floatp number)
      (cons consp sequence)
      (sequence sequencep t)
      (list listp cons sequence)
      (vector vectorp  sequence)
      (character characterp t)
      (symbol symbolp t)
      (keyword keywordp symbol)
      (function functionp t)
      (array arrayp t)
      (string stringp vector)
      (atom atom )
      (das das/structure-pred t)
      (null null list)
      (t atom)
      (nil atom null) ))

  (map 'nil
       (lambda (typedef)
         (setf (gethash (car typedef) *das-types*)
               (make-das-typedef :type (car typedef)
                                 :predicate (cadr typedef)
                                 :supertype (jscl::ensure-list (caddr typedef))
                                 :class (cadddr typedef))))
       *das-basic-types*))

;;; tiny type-of
(export '(das::the-type-of))

(defun the-type-of (value)
  (unless value
    (return-from the-type-of 'null))
  (cond
    ((null value) 'null)
    ((eql value t) 'boolean)
    ((integerp value) 'integer)
    ((floatp value) 'float)
    ((stringp value) 'string)
    ((functionp value) 'function)
    ((keywordp value) 'keyword)
    ((symbolp value) 'symbol)
    ((characterp value) 'character)
    ((hash-table-p value) 'hash-table)
    ((consp value) 'cons)
    ((das/standard-object-p value)
     (cdr (das/standard-object-type-kid value)))
    ((vectorp value) 'vector)
    ((arrayp value) 'array)
    (t (das/typer-raise +wtf+ value)) ))

;;; class-of
(defun the-class-of (type)
  (das-typedef-class (find-typedef type) ))

(export '(das::the-typep))
;;; tiny typep
(defun the-typep (value type) 
  (when (eq type nil)(return-from the-typep nil))
  (when (eq type t)  (return-from the-typep t))
  (if (symbolp type)
      (let ((def (das/find-typedef type)))
        (cond (def (funcall (das-typedef-predicate def) value))
              (t (das/typer-raise +typedef-not-exists+ type ))))
      (das/typer-raise +symbol-expected+ type) ))

;;; tiny subtypep. only internal form
(defun %das-inherit-types (supers)
  (mapcan #'(lambda (c)
              (nconc (list c)
                     (%das-inherit-types
                      (das-typedef-supertype (das/find-typedef c)))))
          supers))

(defun %build-inherit-types (for)
  (%das-inherit-types (das-typedef-supertype (das/find-typedef for))))

(defun das/subtypep (type1 type2)
  (find type2 (%build-inherit-types type1)))

(in-package :cl-user)

;;;EOF
