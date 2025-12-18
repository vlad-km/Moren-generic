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


;;; errors
(defvar *dasgen-meser*)
(setq *dasgen-meser*
      #(
        "DAS: Generic ~a not found."                                                 ;; das/gf-get-for
        "DAS: Wrong expr syntax: ~a."                                                ;; das/lambda-counter
        "DAS: Invalid typename ~a."                                                  ;; das/gf-mask-spwc-parser-type
        "DAS: Cant recognize slot ~a."                                               ;; das/lambda-mask
        "DAS: Specializers form ~a not implemented."                                 ;; das/lambda-mask
        "DAS: Method lambda list: ~a~%      ~a~%Generic lambda list: ~a~%       ~a." ;; das/gf-check-lambda
        ))

(defconstant +generic-not-exists+ 0)
(defconstant +wrong-expression+ 1)
(defconstant +wrong-type-name+ 2)
(defconstant +cant-recognized+ 3)
(defconstant +specializers-form+ 4)
(defconstant +method-lambda-list-expected+ 5)

(defun das/gener-raise (n-error &rest arguments)
  (apply 'error (push (aref *dasgen-meser* n-error) arguments)))


;;; all generic's store
(defvar *das-gfd* nil)
(setq *das-gfd*  (make-hash-table :test #'equal))

(defun das/store-gfd (name gf)
    (setf (gethash name *das-gfd*) gf))

(defun das/gf-get-for (name)
  (let ((g (gethash name *das-gfd*)))
    (unless g (das/gener-raise  +generic-not-exists+ name))
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
            (t (das/gener-raise +wrong-expression+  slot))))
    count))


;;; very simple specialize parser
(defun das/gf-mask-spec-parser-type (expr)
  (if (symbolp expr)
      (return-from das/gf-mask-spec-parser-type (das-typedef-type (das/find-typedef expr)))
      (das/gener-raise +wrong-type-name+ expr)))

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
               ((0 1) (das/gener-raise +cant-recognized+  slot))
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
               (otherwise (das/gener-raise +specializers-form+ slot)))
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

;;; note: labels -> flet 
(defun das/gf-find-optional-args (lambda-list)
  (let* ((len (length lambda-list))
         (rest (position '&rest lambda-list))
         (optional  (position '&optional lambda-list))
         (key  (position '&key lambda-list)))
    (flet ((%counter (pos)
               (cond (pos  (das/gf-optionals-counter (subseq lambda-list (1+ pos) len)))
                     (t 0))))
      (values (%counter rest) (%counter optional) (%counter key)))))

;;; das/gf creator

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
(deftype non-empty-list () `(satisfies jscl::true-list-p))

(defun das/gf-create (name lambda-list)
  (check-type name symbol)
  (check-type lambda-list non-empty-list)
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

;;; note: fix labels -> flet
(defun das/root-dgf (gfname &rest vars)
  (let* ((gf (das/gf-get-for gfname))
         (mhd)
         (args)
         (argvals))
    (flet ((%invoke-by (method-mask)
               ;; todo: catch error with gethash nil
               (apply (das-gf-method-fn (gethash method-mask mhd)) args ) ))
      ;; prepare methods and args for invoke call
      ;; get methods table
      ;;(setq mhd (das-gf-methods gf))
      ;;(setq args (first vars))
      ;;(setq argvals (subseq args 0 (das-gf-mask-len gf)))

      (setq mhd (das-gf-methods gf)
            args (first vars)
            argvals (subseq args 0 (das-gf-mask-len gf)))

      (dolist (mask (das-gf-specialite gf))
        (if (%every-identity (%type-value-compare argvals mask) )
            (return-from das/root-dgf (%invoke-by mask)) )))))


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
;;; Return prop list with  method lambda list parameters


(defun das/gf-check-lambda  (gf arglist)
  (let ((method-lambda (das/gf-parse-lambda-list arglist)))
    (cond ((or (/= (das-gf-rest-count gf)
                   (getf method-lambda :rest-count))
               (/= (das-gf-optional-count gf)
                   (getf method-lambda :optional-count))
               (/= (das-gf-key-count gf)
                   (getf method-lambda :key-count))
               (/= (das-gf-mask-len gf)
                   (getf method-lambda :mask-len)))
           ;; error
           (das/gener-raise  +method-lambda-list-expected+
                             (getf method-lambda :lambda-mask)
                             arglist
                             (das-gf-lambda-mask gf)
                             (das-gf-arglist gf)))
          ;; aprove
          (t method-lambda))))



;;; Sort function for specialite
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
            (setf (das-gf-specialite gf) (das/gf-sort-specialite (das-gf-specialite gf)))
            ) ))
    ;; In this place to compile the method body
    ;; For connecting call-next environment
    (setf (das-gf-method-fn md) method-body)
    ;; Store method descriptor
    ;; key = hash (mask)
    (setf (gethash hash  mht) md )
    ))


;;; method macro form
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


;;; (%check-inside-generic-methods '((:method (a b) t) (:meth nil) (:method (a b c))) '(a b c))
;;; => nil
;;; (%check-inside-generic-methods '((:method (a b c) t) (:method (a b c) nil) (:method (a b c) (lambda nil nil))) '(a b c))
;;; => t
(defun %check-generic-methods-forms (methods-list generic-args-list)
  ;;(print (list :check methods-list generic-args-list))
  (let ((length-ga (list-length generic-args-list))
        (min-prop-len 2)
        (mask))
    (dolist (it methods-list)
      ;;(print (list :it it))
      ;;(print (list :I  min-prop-len (list-length it) :cond (jscl::proper-list-p it)))
      ;;(print (list :II (eq (car it) :method)))
      ;;(print (list :III (length (cadr it)) :cond  (eq (list-length (cadr it)) length-ga)))
      (when (and
             ;; check what:
             ;; i. :method form is proper list (without conses)
             (jscl::proper-list-p it)
             ;; ii. :method firstly element
             (eq (car it) :method)
             ;; iii. :method args list eq :generic arg list
             (typep (cadr it) 'list)
             (jscl::proper-list-p (cadr it))
             (eq (list-length (cadr it)) length-ga))
        ;; minimal method declaration: (:method (a b c) [nil]) length range [2...n]
        ;; first element: eq `:method
        ;; second element: i. list  ii. length eq generic-args-list
        ;; method body not checked
        (push t mask)))
    ;;(print (list :mask mask))
    (if mask
        (cond ((eq (list-length mask) (length methods-list)) t)
              (t nil)))))

;;; generic forms:
;;; i. (generic name (a b c))
;;; ii. (generic name (a b c)
;;;        (:method (a b c) :general)
;;;        (:method ((a list) b c) :first-list)
;;;        (:method (a (b list) c) :second-list))
(defmacro das!generic (name (&rest vars) &rest others)
  (let* ((gf (das/gf-create name vars ))
         ;;(vars-length (list-length vars))
         (methods-p (when others (%check-generic-methods-forms others vars)))
         (methods)
         (generic-func-name (intern (symbol-name name))))
    (if (and (null methods-p) others)
        (das/gener-raise +wrong-expression+ others))
    (jscl::fset generic-func-name
                (fdefinition (lambda (&rest args) (das/root-dgf generic-func-name args))))
    (das/store-gfd generic-func-name gf)
    (when methods-p
      (setq methods
            (jscl::with-collect
                (dolist (it others)
                  (jscl::collect `(das!method ,name ,(cadr it) ,@(rest (rest it))))))))
    `(progn
       ,@methods
       ',generic-func-name)
    ))

;;; EOF
