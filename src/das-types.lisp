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

;;; DAS TYPES.  Very simple types system

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

;;; TYpe definition structure
(defstruct (das-typedef (:type vector) :named) type supertype predicate class)

(defvar *das-types* nil)
(setq *das-types* (make-hash-table :test #'equal))

(export '(def-type))
(defun def-type (&rest typedef)
  (setf (gethash (car typedef) *das-types*)
        (make-das-typedef :type (car typedef)
                          :predicate (cadr typedef)
                          :supertype (caddr typedef)
                          :class (cadddr typedef))))

;;; Find deftype for symbol type
(defun das/find-typedef (type)
  (let ((ok (gethash type *das-types*)))
    (if ok ok (error "DAS: ~a not a type name." type))))

;;; DAS TYPES PREDICATE
;;;
;;; ht - hash table
;;; generic form = (hash-table mem fn &others)
;;; properties
;;;    (car ht) = hash-table
;;;    (length ht) = 3
;;;    (cadr ht) = function

(defun das/hash-table-p (value)
  (and (consp value)
       (eq (car value) 'hash-table)
       (= (length value) 3)
       (functionp (cadr value)) ) )


(defun das/standard-object-p (obj)
  (and (storage-vector-p obj)
       (> (length obj) 0)
       (consp (storage-vector-ref obj 0))
       (= (length (storage-vector-ref obj 0)) 2) ))

(defun das/standard-object-p (obj)
  (and (storage-vector-p obj)
       (> (length obj) 0)
       (consp (storage-vector-ref obj 0))
       (member (car (storage-vector-ref obj 0)) '(structure instance))
       t ))

;;; NOTE: WHAT IS ?
(defun das/standard-object-type-kid (obj)
  (storage-vector-ref obj 0))

;;; note: drop it
;;;(export '(das/numberp das/characterp das/symbolp das/functionp))

(defun das/numberp (value) (numberp value))
(defun das/characterp (value) (characterp value))
(defun das/symbolp (value) (symbolp value))
(defun das/functionp (value) (functionp value))


;;;
;;; types/classes hierarchy
;;;
;;; t             atom
;;; character      t
;;; function       t
;;; array          t
;;; sequence       t
;;; number         t
;;; symbol         t
;;; vector         array sequence
;;; string         vector
;;; list           sequence
;;; cons           list
;;; null           list
;;; float          number
;;; integer        number
;;; hash-table     t
;;;

;;; todo: fix it
(let ()
  (defparameter *das-basic-types*
    '((hash-table das/hash-table-p t)
      (number das/numberp t)
      (integer integerp number)
      (float floatp number)
      (cons consp sequence)
      (sequence sequencep t)
      (list listp cons sequence)
      (vector vectorp  sequence)
      (character das/characterp t)
      (symbol das/symbolp t)
      (keyword keywordp symbol)
      (function das/functionp t)
      (array arrayp t)
      (string stringp vector)
      (atom atom)
      (das das/structure-pred t)
      (null null list)
      (t atom)
      (nil atom null) ))

  (map 'nil
       (lambda (typedef)
         (setf (gethash (car typedef) *das-types*)
               (make-das-typedef :type (car typedef)
                                 :predicate (cadr typedef)
                                 :supertype (ensure-list (caddr typedef))
                                 :class (cadddr typedef))))
       *das-basic-types*))

;;; Some das-type-of
;;; todo: Fix it
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
    ((das/hash-table-p value) 'hash-table)
    ((consp value) 'cons)
    ((das/standard-object-p value) (cdr (das/standard-object-type-kid value)))
    ((vectorp value) 'vector)
    ((arrayp value) 'array)
    (t (error "wtf ? ~a" value)) ))

;;; class-of
;;; NOTE: ???

(defun the-class-of (type)
  (das-typedef-class (find-typedef type) ))

;;;
;;; typep
;;; typep object type-specifier &optional environment
;;;      => generalized-boolean
;;;
;;; Returns true if object is of the type specified by type-specifier;
;;; otherwise, returns false.
;;;
;;; (das/typep (make-array 0 :element-type 'Ax) 'vector) =>  true
;;; (das/typep 12 'integer) =>  true
;;; (das/typep nil t) =>  true
;;; (das/typep nil nil) =>  false

(export '(the-typep))

;;;
;;; typep 11 'integer
;;;       'y 'symbol

;;; todo: Fix it
(defun the-typep (value type)
  (when (eq type nil)(return-from the-typep nil))
  (when (eq type t)  (return-from the-typep t))
  (if (symbolp type)
      (let ((def (das/find-typedef type))
            (fn))
        (if def
            (if (setq fn (das-typedef-predicate def))
                (funcall fn value)
                (error "Invalid typedef ~a." type))
            (error "Cant find typedef ~a." type )))
      (error "Invalid type ~a." type) ))

;;; subtypep
;;; subtypep type-1 type-2 &optional environment => subtype-p, valid-p
;;;
;;; From CL
;;;
;;; If type-1 is a recognizable subtype of type-2, the first value is true. Otherwise,
;;; the first value is false, indicating that either type-1 is not a subtype of type-2,
;;; or else type-1 is a subtype of type-2 but is not a recognizable subtype.
;;;
;;; A second value is also returned indicating the `certainty' of the first value. If this value is true,
;;; then the first value is an accurate indication of the subtype relationship.
;;; (The second value is always true when the first value is true.)
;;;
;;; The next figure summarizes the possible combinations of values that might result.
;;;
;;; Value 1  Value 2  Meaning
;;; true     true     type-1 is definitely a subtype of type-2.
;;; false    true     type-1 is definitely not a subtype of type-2.
;;; false    false    subtypep could not determine the relationship,
;;;                   so type-1 might or might not be a subtype of type-2.

;;; all supertypes for type

(defun %das-inherit-types (supers)
  (mapcan #'(lambda (c)
              (nconc (list c)
                     (%das-inherit-types
                      (das-typedef-supertype (das/find-typedef c)))))
          supers))

(defun %build-inherit-types (for)
  (%das-inherit-types (das-typedef-supertype (das/find-typedef for))))

;;; return supertype specializer if type1 is subtype type2
;;; nil if not
(defun das/subtypep (type1 type2)
  (find type2 (%build-inherit-types type1)))


;;;EOF
