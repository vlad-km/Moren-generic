;;; -*- mode:lisp;  coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  
             )   (                    Copyright © 2017,2025  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/dasgen
            )     (                   2025, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  Moren  
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

(in-package :das)

(defvar *jso-diagnos+ nil)
(setq *jso-diagnos+
      #(
        "DAS: wrong type `~a`. `symbol` expected."
        "DAS: wrong structure slot `~a` syntax."
        "DAS: something went wrong: ~a."
        ))

(defconstant +jo-key-wrong-type+ 0)
(defconstant +jo-key-wrong-syntax+ 1)
(defconstant +jo-went-wrong+       2)


(defun %jso-raise-error (num &rest args)
  (apply 'error (aref *jso-diagnos+ num) args))

;;; (a (b t)) => ("a" a "b" b)
(defun %compose-object-keys (slots)
  (let* ((q))
    (flet ((%composer (s)
             (push (ffi::%nc s) q)
             (push s q)))
      (dolist (it slots)
        (cond ((jscl::true-list-p  it)
               (let ((name (car it)))
                 (cond  ((symbolp name) (%composer name))
                        (t (%jso-raise-error +jo-key-wrong-type+ name)))))
              ((symbolp it) (%composer it))
              (t (%jso-raise-error +jo-key-wrong-syntax+ it))))
      (reverse q))))

;;; (a (b nil)) => (a "A") (b "B")           
(defun %compose-get-set-names (slots)
  (let* ((q))
    (flet ((%composer (s) (push (list s (ffi::%nc s)) q)))
    (dolist (it slots)
      (cond ((symbolp it) (%composer it))
            ((consp it) (%composer (car it)))))
      (reverse q))))


;;; structure without copier
;;; structure base: #j:Object
(defun %das-struct-generator (kind options slots)
  (let* ((constructor (cadr (assoc :constructor options)))
         (opt-key (cadr (assoc :form options)))
         (shady-key (cadr (assoc :shady options)))
         (mak-obj-key-val (%compose-object-keys slots))
         (get-set-names (%compose-get-set-names slots))
         (option (if opt-key opt-key '&optional))
         (maker)
         (makname (if constructor
                      (intern (symbol-name constructor))
                      (intern (jscl::concat "MAKE-" (symbol-name `,kind)))))
         (getter)
         (predicate)
         (position 0)
         (q))
    (unless (jscl::memq option '(&key &optional)) (%jso-raise-error +jo-went-wrong+ options))
    (setq maker
          `(defun ,makname (,option ,@slots)
             (let* (({} (apply 'ffi:make-obj (list ,@mak-obj-key-val)))
                    (shady-p ,shady-key))
               (when shady-p
                 (ffi:setprop ({} "_observer") (lambda nil (ffi:with-this self (ffi:obj-list self))))
                 (ffi:setprop ({} "_method") (lambda (name &rest args)
                                               (ffi:with-this self
                                                 (let ((fn (ffi:getprop self name)))
                                                   (unless fn (error "No such method ~a" name))
                                                   (apply fn self args)))))
                 (ffi:setprop ({} "_make") (lambda (name value) (ffi:with-this self (ffi:setprop (self name) value)))))
               (ffi:setprop ({} "__das_type__") +structure-das-object-tag+)
               (ffi:setprop ({} "__das_type_name__") ',kind)
               {} )))

    (dolist (it get-set-names)
      (setq getter (intern (jscl::concat (symbol-name kind) "-" (symbol-name (car it))))
            predicate (intern (jscl::concat (symbol-name kind) "-P")))
      (push `(defun ,getter ({})(ffi:getprop {} ,(cadr it))) q)
      (push `(defun (setf ,getter) (value {})
               (ffi:setprop ({} ,(cadr it)) value) value)
            q)
      (push `(defun ,predicate ({}) (if (eq (ffi:getprop {} "__das_type__" ) +structure-das-object-tag+)
                                        (eq (ffi:getprop {} "__das_type_name__") ,predicate)))
            q)
    (values maker (reverse q))))

(defun @structure-name-p (p) (if (eq (ffi:getprop p "__das_type__") +structure-das-object-tag+) (ffi:getprop p "__das_type_name__")))
(defun @structure-name (p) (ffi:getprop p "__das_type_name__"))
(defun @structure-p (p) (eq (ffi:getprop p "__das_type__") +structure-das-object-tag+))

(export '(das::@structure))

(defmacro @structure (name-options &rest slots)
  (let* ((name-options (jscl::ensure-list name-options))
         (name (car name-options))
         (options (rest name-options)))
    (multiple-value-bind (maker accessors) (%das-struct-generator name options slots)
      `(progn
         ,maker
         ,@accessors
         ',name))))

(in-package :cl-user)

;;; EOF
