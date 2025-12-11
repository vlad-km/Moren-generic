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
              //
             ((
              \)
|#


;;; all generic's store
(defvar *das-gfd* nil)
(setq *das-gfd*  (make-hash-table :test #'equal))

(defun das/store-gfd (name gf)
    (setf (gethash name *das-gfd*) gf))

(defun das/gf-get-for (name)
  (let ((g (gethash name *das-gfd*)))
    (unless g (error "DAS: Generic ~a not found." name))
    g))


;;; DAS GF descriptor
(defstruct (das-gf (:type vector) :named)
            name  arglist lambda-len  lambda-mask   mask-len
            rest-count optional-count  key-count specialite
            methods )

;;; generic as function
(defstruct (das-gf-method (:type vector) :named)
         name lambda-len lambda-mask mask-len
         lambda-vars rest-count  optional-count  key-count fn primary
         around before after)


(defconstant *das-gf-mask-stop-tokens* '(&rest &optional &key))

(defun das/lambda-counter (lambda-list)
  (let ((count 0))
    (dolist (slot lambda-list)
      (cond ((atomp slot)
             (if (find slot *das-gf-mask-stop-tokens*)
                 (return-from das/lambda-counter count)
                 (incf count)) )
            (t (error (concat "DAS: Dont recognized expr ~a." slot))))
      count)))

;;; very simple specialize parser
(defun das/gf-mask-spec-parser-type (expr)
  (if (symbolp expr)
      (return-from das/gf-mask-spec-parser-type (das-typedef-type (das/find-typedef expr)))
      (error "DAS: Invalid typename ~a." expr)))

;;;   return mask count args arglist without typespec
(defun das/lambda-mask (lambda-list)
  (let ((count 0)
        (reqvars)
        (mask '()))
    (dolist (slot lambda-list)
      ;; Symbol
      (cond ((atom slot)
             ;; If in stop-list, returned
             (if (jscl::memq slot *das-gf-mask-stop-tokens*)
                 (return-from das/lambda-mask (values (reverse mask) count (reverse reqvars)))
                 ;; else mark mask
                 (progn
                   (push slot reqvars)
                   (push t mask)
                   (incf count))) )
            ;; cons
            ((consp slot)
             (case (length slot)
               ;; May be nil
               ((0 1) (error "DAS: Cant recognize slot ~a." slot))
               ;; or class specializer
               ;;    type specializer (var list|integer|float)
               (2 (let ((var (car slot))
                        (type (cadr slot)))
                    ;;(print (list 'mask slot (car slot) (cadr slot)))
                    (setq type (das/gf-mask-spec-parser-type type))
                    (push var reqvars)
                    (incf count)
                    (push type mask) ))
               ;; other conses dont implemented
               (otherwise (error  "DAS: Specializers form ~a not implemented." slot)))
             ) ))
    (values (reverse mask) count (reverse reqvars))))

;;; optionals counter
(defun das/gf-optionals-counter (sub-lambda-list)
  (let ((count 0))
    (dolist (slot sub-lambda-list)
      (when (find slot *das-gf-mask-stop-tokens*)
          (return-from das/gf-optionals-counter count))
      (setq count (1+ count)) )
    count))

;;; todo: labels ??
;;; note: sunseq
;;; todo: wrong code. lambda-list mb empty
(defun das/gf-find-optional-args (lambda-list)
  (let* ((len (length lambda-list))
         (rest (position '&rest lambda-list))
         (optional  (position '&optional lambda-list))
         (key  (position '&key lambda-list)))
    (labels ((%counter (pos)
               (cond (pos  (das/gf-optionals-counter (subseq lambda-list (1+ pos) len)))
                     (t 0))))
      (values (%counter rest) (%counter optional) (%counter key)))))

;;; das/gf creator

;;; with remove specializers
#+nil
(defun das/gf-parse-lambda-list (lambda-list)
  (multiple-value-bind (mask lmask vars)
      (das/lambda-mask lambda-list)
    (multiple-value-bind (rest optional key)
        (das/gf-find-optional-args lambda-list)
      (list
       :lambda-len (length lambda-list)
       :lambda-mask mask
       :mask-len lmask
       :lambda-vars (if (= lmask (length lambda-list))
                        vars
                        (append vars (subseq lambda-list lmask)))
       :rest-count rest
       :optional-count optional
       :key-count key))))


;;; version for type specializers
(defun das/gf-parse-lambda-list (lambda-list)
  (multiple-value-bind (mask lmask vars)
      (das/lambda-mask lambda-list)
    (multiple-value-bind (rest optional key)
        (das/gf-find-optional-args lambda-list)
      (list
       :lambda-len (length lambda-list)
       :lambda-mask mask
       :mask-len lmask
       :lambda-vars (if (= lmask (length lambda-list))
                        vars
                        (append vars (subseq lambda-list lmask)))
       :rest-count rest
       :optional-count optional
       :key-count key))))

;;; Create gf descriptor from  lambda list
;;; Called from DEF!GENERIC
;;; Used das/lambda-mask
;;;      das/gf-find-optional-args
(defun das/gf-create (name lambda-list)
  (let ((gf))
    (multiple-value-bind (mask lmask vars) (das/lambda-mask lambda-list)
      (multiple-value-bind (rest optional key)
          (das/gf-find-optional-args lambda-list)
        (setq gf (make-das-gf
                  :name name
                  :arglist lambda-list
                  :lambda-len (length lambda-list)
                  :lambda-mask mask
                  :mask-len lmask
                  :rest-count rest
                  :optional-count optional
                  :key-count key))))
    gf))


;;; DAS/GF ROOT METHOD

(defun %every-identity (seq)
  (dolist (elt seq)
    (unless (identity elt)
      (return-from %every-identity nil)))
  t)

(defun %type-value-compare (vals mask)
  (let ((res))
    (dotimes (idx (length vals))
      (push (the-typep (nth idx vals) (nth idx mask)) res ))
    (reverse res)))

;;; note: fix it
(defun das/root-dgf (gfname &rest vars)
  (let* ((gf (das/gf-get-for gfname))
         (mhd)
         (args)
         (argvals))
    (labels ((%invoke-by (method-mask)
               ;; todo: catch error with gethash nil
               (apply (das-gf-method-fn (gethash method-mask mhd)) args ) ))
      ;; prepare methods and args for invoke call
      ;; get methods table
      (setq mhd (das-gf-methods gf))
      (setq args (first vars))
      (setq argvals (subseq args 0 (das-gf-mask-len gf)))
      ;; search method by mask
      (dolist (mask (das-gf-specialite gf))
        (if (%every-identity (%type-value-compare argvals mask) )
            (return-from das/root-dgf (%invoke-by mask)) )))))

;;; DAS!GENERIC MACRO

;;; todo: fix it (fset & others)
(defmacro das!generic (name (&rest vars))
  (let ((gf (das/gf-create name vars ))
        (fname)
        (fn (intern (symbol-name (gensym (jscl::concat  "DGF-" (princ-to-string name)))))) )
    (setq fname (intern (symbol-name `,name)))
    `(progn
       ;; Define gf accessor with uniq name DGF-generic-name--bla-bla-bla
       (defun ,fn (&rest args) (das/root-dgf ',fname args))
       ;; Set function symbol
       (jscl::fset ',fname (fdefinition ',fn))
       ;; Store descriptor to global table
       (das/store-gfd ',fname ',gf)
       ',fname)
    ))

;;; check method lambda list
;;; only for DEF!METHOD
;;;
;;; lambda check list
;;;    1 - count required lambda arguments
;;;    2 - count optional lambda arguments
;;;    3 - count rest arguments
;;;    4 - count key arguments
;;;
;;; Remove type specializers from required arguments
;;;
;;; Return prop list with  method lambda list parameters

;;; todo: fix the error message
(defun das/gf-check-lambda  (gf arglist)
  (let ((method-lambda (das/gf-parse-lambda-list arglist)))
    (if (or (/= (das-gf-rest-count gf)
                (getf method-lambda :rest-count))
            (/= (das-gf-optional-count gf)
                (getf method-lambda :optional-count))
            (/= (das-gf-key-count gf)
                (getf method-lambda :key-count))
            (/= (das-gf-mask-len gf)
                (getf method-lambda :mask-len)))
        (error (format nil
                       "DAS: Method lambda list: ~a~%      ~a~%Generic lambda list: ~a~%       ~a."
                       (getf method-lambda :lambda-mask)
                       arglist
                       (das-gf-lambda-mask gf)
                       (das-gf-arglist gf))))
    method-lambda) )

;;; add new method
;;;
;;; Sort function for specialite
;;; Use:
;;;    at clos/gf-add-method
;;;    most specified method mask must be first
;;;    in method mask list for clos/gf-root
;;; <= ((t integer t) (t integer integer) (t t t))
;;; => ((t integer integer) (t integer t) (t t t))

(defun das/sort-method-mask-comparator (x y)
  (let* ((mixt (mapcar (lambda (xx yy) (cons xx yy)) x y ))
         (winx (count-if (lambda (pair) (das/subtypep (car pair) (cdr pair)))
                         mixt))
         (winy (count-if (lambda (pair) (das/subtypep (cdr pair) (car pair)))
                         mixt)))
    (cond ((< winx winy) 1)
          ((= winx winy) 0)
          (t -1))))


;;; todo: fix it
(defun das/gf-sort-specialite (lst)
  (let* ((jarray (jscl::list-to-vector lst))
         (sort (lambda (fn)
                 (funcall ((jscl::oget jarray "sort" "bind") jarray fn)))))
    (funcall sort #'das/sort-method-mask-comparator)
    (jscl::vector-to-list jarray)))


;;; add new method definition
(defun das/gf-add-method (gf method-lambda method-body)
  (let* ((md (apply #'make-das-gf-method method-lambda))
         (mht (das-gf-methods gf))
         (hash (getf method-lambda :lambda-mask)) )
    ;; its first call for this generic
    (unless mht
      ;; create mask-method hash table
      (setq mht (make-hash-table :test #'equal))
      ;;(set-das-gf-methods gf mht)
      (setf (das-gf-methods gf) mht))
    ;; update method table and specialite
    (multiple-value-bind (mask ok) (gethash hash mht)
      (if ok
          ;; Method with such a mask already exists
          ;; You just need to change the body function of the method
          ;; specialite list do not change
          (warn "DAS: Method ~a redefinition." (das-gf-name gf))
          (progn
            ;; This is a unique mask
            ;; Remember it in the specialite list and sort the list
            (push (getf method-lambda :lambda-mask) (das-gf-specialite gf))
            ;; bug: (set-das-gf-specialite gf (das/gf-sort-specialite (das-gf-specialite gf)))
            (setf (das-gf-specialite gf) (das/gf-sort-specialite (das-gf-specialite gf)))
            ) ))
    ;; In this place to compile the method body
    ;; For connecting call-next environment
    ;; bug: (set-das-gf-method-fn md method-body)
    (setf (das-gf-method-fn md) method-body)
    ;; Store method descriptor
    ;; key = hash (mask)
    (setf (gethash hash  mht) md )
    ))


;;; DAS!METHOD macro
(defmacro das!method (name (&rest arglist) &rest body)
  (let* ((gfname (intern (symbol-name `,name)))
         (method-lambda (das/gf-check-lambda (das/gf-get-for gfname) arglist))
         (arg-list)
         (a!list))
    (setq a!list (getf method-lambda :lambda-vars))
    `(das/gf-add-method
      (das/gf-get-for ',gfname)
      ',method-lambda
      #'(lambda  ,a!list . ,body))
    ) )

;;; EOF
