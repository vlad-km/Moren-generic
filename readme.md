# DAS- tiny implementation of CommonLisp Generic Function for Moren environment


## Disclaime

-  This Code execute in `zero safety` mode.
-  Code written in dialect `non-compliant` ANSI Common Lisp
-  Using `CLOS` into the  code, will slow it down to its `CLOS level`.
-  This code is written for use in a `narrow domain`.


## das:generic

*Must be declared before method's definition*

*defgeneric syntax* `:method`, `:documentation` and others, *not implemented*.

*das:generic* ::= *name* *args* *optionals*

*name*::= `symbol`

*args* :: `symbol` | `symbol` ... `symbol`

*optionals* ::= `&optional` | `&keyword` | `&rest`


```lisp
   (das:generic name (x y z))
   (das:generic name (x &optional a b c))
   (das:generic name (pip &key x y z))
```

## das:method

*defmethod syntax* `:before`,`:after`,`:around`  *not implemented*.

*das:method* *function-name*  *specialized-lambda-list*  *forms*)

*function-name*::= `symbol`

*specialized-lambda-list*::= *vars* *optional* | *vars* *typed-vars* *optionals*

*vars* ::= `symbol` ... `symbol`

*typed-vars* ::= (`symbol` *type-name*) ... (`symbol` *type-name*)

*type-name* ::= `integer` | `float` | `character` | `string` | `list`  | `consp` | `function` 
| `hash-table` | `vector` | `symbol` | `keyword`  | *any-type-name*

*optionals* ::= *only &optional/&key/&rest forms*

*any-type-name* ::= any lisp entity that has a `predicate`, and registered with the function `das:def-type`


```lisp
    (das:generic compare-slots (x y))    
    (das:method compare-slots ((x integer) (y integer)) )
    (das:method compare-slots ((x integer) y ) )
    (das:method compare-slots ((x list) (y list)) )

    (defstruct (negation (:constructor negation (expr)) :named (:type vector)) expr)   
    (das:def-type 'negation (lambda (p) (negation-p p)))

    (defstruct (addition (:constructor addition (left right)) :named (:type vector)) left right)
    (das:def-type 'addition (lambda (p) (addition-p p)))

    (das:generic evaluate (expr))
    (das:method evaluate ((expr negation))
            (- (evaluate (negation-expr expr))))
    (das:method evaluate ((expr addition))
            (+ (evaluate (addition-left expr))
               (evaluate (addition-right expr))))
     (das:method evaluate ((expr number))
            expr)

     (evaluate (addition 5 (negation -5)))

```

## das:def-type

*das:def-type* :*type* :*predicate* 

*type* ::= `symbol`

*predicate* ::= 'symbol` | `function`

```lisp
(defstruct (stub (:type vector) :named) (fn (error "undef function") :type function))
(das:def-type :type 'stub :predicate (lambda (x) (stub-p x)))

(das:generic anything (x y))
(das:method anything (x y) (format t "Don't know what do this tool: ~a  with this object: ~a~%" x y))
(das:method anything ((than stub) (with vector)) (ffi:call (with "forEach") (stub-fn than)))
(das:method anything ((than function) (with vector)) (ffi:call (with "forEach") than))

(setq heap (make-array '(5) :initial-element (ffi:ref "undefined")))
(setq ps (make-stub :fn (lambda (x) (format t "And what do with it ~a?~%" x))))
(anything ps heap)
(anything t (list 1 2 3))

(das:method anything ((than function) (with list))
  (let (result)
      (dolist (it with (reverse result)) (push (funcall than it) result))))

(anything 'integerp (list 1 2 3 4))

```

## das:the-type-of

*das:the-type-of* `object`

```lisp
(das:the-type-of 1) => t
(das:the-type-of nil) => nil
(das:the-type-of ps) => stub
```

## das:the-typep

*das:the-typep* `object` `type-name`

*object* ::= `<any entitie>`

*type-name* ::= `symbol`

```lisp
(das:the-typep (list 1 2 3) 'list) => t
(das:the-typep ps 'stub) => t
(das:def-type :name 'list-triple 
              :predicate (lambda (x) 
                           (when  (listp x) (eq (length x) 3))))
(das:the-typep '(1 2 3) 'list-triple) => t
```

## Compilation

```lisp

(setq bin (make-array 0 :fillpointer 0)) 
(load "src/package.lisp" :hook bin)
(load "src/das-types.lisp" :hook bin)
(load "src/das-generic.lisp" :hook bin :output "./dasgen.js")

;; further use:  (require "./dasgen.js") or html:<script >

```

### Copyright 2017,2025 @vlad-km


