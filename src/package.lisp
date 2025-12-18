;;; -*- mode:lisp;  coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  
             )   (                    Copyright © 2017,2018,2025  @vlad-km
            /     \                   2017, Original https://github.com/vlad-km/moren-electron
            )     (                   2025, Code redesign
           /       \                  Electron >= electron@21.2.2
           \       /                  Moren  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; drop old package with (let) & (hash)
  (when (find-package "DAS") (delete-package "DAS"))
  (unless (find-package "DAS") (defpackage :das  (:use :cl))))


(if (not (jscl::memq :das-generic-v1 *features*))
    (push :das-generic-v1 *features*))

;;; EOF
