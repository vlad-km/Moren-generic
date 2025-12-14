# DAS- simple implementation of CommonLisp Generic Function for Moren Environment


## das:generic
Must be declared before method definitions.

*das:generic* ::= *name* *args* *optionals*

*name*::= `symbol`

*args* :: `symbol` | `symbol` ... `symbol`

*optionals* ::= `&optional` | `&keyword` | `&rest`

*Optionals* : `:method`, `:documentation` and others, not implemented.

```lisp
   (das:generic name (x y z))
   (das:generic name (x &optional a b c))
   (das:generic name (pip &key x y z))
```

## das:method

```lisp
(das:method function-name  specialized-lambda-list  form*)
``` 

*function-name*::= `symbol`

*specialized-lambda-list*::= ({var | (var parameter-specializer)}* 
                              &optional var-form* | &key var-form*  | &rest var)

*release limitation*::= *only &optional / &key / &rest forms*.

*parameter-specializer*::= `integer` | `float` | `character` | `string`
                           | `list`  | `consp` | `function`
                           | `hash-table` | `vector` | `symbol` | `keyword` 
                           | `any-type-name`

*any-type-name* ::= any lisp entity that has a `predicate`, and registered with the function `das:def-type`


```lisp
    (das:generic compare-slots (x y))    
    (das:method compare-slots ((x integer) (y integer)) )
    (das:method compare-slots ((x integer) y ) )
    (das:method compare-slots ((x list) (y list)) )

    (defstruct (negation (:constructor negation (expr)) :named (:type vector)) expr)   
    (def-type 'negation (lambda (p) (negation-p p)))

    (das:defstruct (addition (:constructor addition (left right)) :named (:type vector)) left right)
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
;; ==> 10

```

## das:def-type



## Compilation

```lisp

(setq bin (make-array 0 :fillpointer 0)) 
(load "package.lisp" :hook bin)
(load "das-types.lisp" :hook bin)
(load "das-generic.lisp" :hook bin :output "./dasgen.js")

;; further use:  (require "./dasgen.js") or html:<script >

```


### Copyright 2017,2025 @vlad-km

#### Have a func!


