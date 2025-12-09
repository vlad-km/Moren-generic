;;; -*- mode:lisp;  coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                    Copyright © 2017,2024  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/dasgen
            )     (                   2024, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

(error "Don't compile this file using the `load` function.
       Execute it manually, step by step, in `repl`.")

;;; Look at:
;;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create#examples

;;; 1. Shape superclass
(setf #j:Shape
       (lambda ()
         (ffi:with-this self
          (ffi:setprop (self "x") 0)
          (ffi:setprop (self "y") 0))))

;;; 2. Superclass method - move
(setf #j:Shape:prototype:move
      (lambda (x y)
        (ffi:with-this self
                       (let ((self-x (ffi:getprop self "x"))
                             (self-y (ffi:getprop self "y")))
                         (setq self-x (+ self-x x) self-y (+ self-y y))
                         (ffi:setprop (self "x") self-x)
                         (ffi:setprop (self "y") self-y)
                         (#j:console:log "Shape moved" self-x self-y)
                         (format t "Shape moved ~a:~a~%" self-x self-y)))))

;;; 3. Rectangle subclass
;;; call superclass constructor
(setf #j:Rectangle
      (lambda () (ffi:with-this self  (#j:Shape:call self))))

;;; 4. Subclass extend superclass
;;;     If you don't set Rectangle.prototype.constructor to Rectangle,
;;;     it will take the prototype.constructor of Shape (parent).
;;;     To avoid that, we set the prototype.constructor to Rectangle (child).

;;; OR
(setf #j:Rectangle:prototype (#j:Object:create #j:Shape:prototype))
(setf #j:Rectangle:prototype:constructor #j:Rectangle)

;;; OR
(setf #j:Rectangle:prototype
      (#j:Object:create  #j:Shape:prototype
                         (ffi:make-obj :constructor (ffi:make-obj :value #j:Rectangle
                                                                  :enumerable nil
                                                                  :writable nil
                                                                  :configurable t))))

;;; Exec
(setf #j:rect (jscl::make-new #j:Rectangle))

(ffi:call (#j:rect "move") 10 15)
;;; => Shape moved 10:15
(setq js-rect #j:rect)

(ffi:call (js-rect "move") 20 32)
;;; => Shape moved 30:47

(ffi::%be-evil "(rect instanceof Shape)")
;;; => T

(ffi:obj-list js-rect)
;;; => (("x" 30) ("y" 47))

(ffi::%be-evil "(rect instanceof Rectangle)")
;;; => T

(ffi::%be-evil "rect instanceof Rectangle")
;;; => T


;;; EOF
