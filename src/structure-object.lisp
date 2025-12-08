;;; -*- mode:lisp;  coding:utf-8 -*-
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

;;;
;;; Structure type Object. Prototype
;;; It's no better than other struct types. It's a pure JS object. It can be useful in combination with FFI:
;;; seal/frozen/defprop.


;;; Perf test: (time (dotimes (i 10000000) (struct-read) (struct-write)))
;;; (defstruct (:type vector))    0.865 sec
;;; (defstruct (:type list))      2.364 sec
;;; (@struct)                     4.966 sec

;;; stucrure without copier/predicate/ it's a pure JS object.
(defun %das-struct-generator (kind options slots)
  (let* ((constructor (cadr (assoc :constructor options)))
         (opt-key (cadr (assoc :form options)))
         (key-names (mapcar (lambda (x) (if (consp x) (car x) x)) slots))
         (obj-keys (mapcar (lambda (x) (let ((y (string-downcase (symbol-name x)))) (list 'list y x))) key-names))
         (option (if opt-key opt-key '&optional))
         (maker)
         (makname (if constructor
                      (intern (symbol-name constructor))
                      (intern (jscl::concat "MAKE-" (symbol-name `,kind)))))
         (getter)
         (position 0)
         (q))
    (unless (jscl::memq option '(&key &optional)) (error "Something went wrong: ~a." options))
    (setq maker
          `(defun ,makname (,option ,@slots)
             (let (({} (ffi:new)))
               (dolist (it (list ,@obj-keys)) (ffi:setprop ({} (car it)) (cadr it)))
               (ffi:setprop ({} "__type__") "structure")
               (ffi:setprop ({} "__type_name__") (string-downcase (symbol-name ',kind)))
               {} )))
    (dolist (it obj-keys)
      (setq getter (intern (jscl::concat (symbol-name kind) "-" (symbol-name (caddr it)))))
      (push `(defun ,getter ({})(ffi:getprop {} ,(cadr it))) q)
      (push `(defun (setf ,getter) (value storage)
               (ffi:setprop (storage ,(cadr it)) value) value)
            q))
    (values maker (reverse q))))

;;; note: for structure slot name's with "-"
;;;       add-sym: (setprop ({} "add-sym") t)
;;;                js: {}.add-sym wrong name
;;;                    {}['add-sym'] 
(defmacro @structure (name-options &rest slots)
  (let* ((name-options (jscl::ensure-list name-options))
         (name (car name-options))
         (options (rest name-options)))
    (multiple-value-bind (maker accessors) (%das-struct-generator name options slots)
      `(progn
         ,maker
         ,@accessors))))


;;; EOF
