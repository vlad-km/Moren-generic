# DAS- simple implementation of CommonLisp Generic Function for Moren Environment


## das:generic

```lisp
   (das:generic name (args*))
```

Must be declared before the definition of the method. Optionals :method, :documentation and others not implemented


### das:method

```lisp
(das:method function-name  specialized-lambda-list  form*)
=> new-method

*function-name*::= symbol

*specialized-lambda-list*::= ({var | (var parameter-specializer)}* 
                              &optional var-form* | &key var-form*  | &rest var)

*release limitation*: only &optional / &key / &rest forms.

*parameter-specializer*: integer | float | character | string 
                       | list  | consp | function
                       | hash-table | vector | symbol | keyword 
                       | any-type-name

*any-type-name* ::= any lisp entity that has a `predicate`, and registered with the function `das:def-type`

```lisp
    (das:method compare-slots ((x integer) (y integer)) )
    (das:method compare-slots ((x integer) y ) )
    (das:method compare-slots ((x list) (y list)) )
```



