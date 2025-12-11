# DAS- simple implementation of CommonLisp Generic Function for Moren Edition JSCL


## Simple implementation of 'defgeneric' and 'defmethod'

Macros: 
- das:generic 
- das:method

Look at `examples/das-examples.lisp` 

### das:generic

```lisp
   (das:generic name (args*))
```

Must be declared before the definition of the method. Optionals :method, :documentation and others not implemented


### das:method

das:method function-name  specialized-lambda-list  form*
=> new-method

function-name::= symbol

specialized-lambda-list::= ({var | (var parameter-specializer-name)}* 
                            [&optional {var | (var [initform ])}*] 
                            [&rest var] 
                            [&key{var | ({var | (keywordvar)} [initform  ])}*]

parameter-specializer: integer | float | character | string 
                       | list  | consp
                       | hash-table | vector | symbol | keyword 
                       | das!structure-name ?????

```lisp
    (das:method compare-slots ((x integer) (y integer)) )
    (das:method compare-slots ((x integer) y ) )
    (das:method compare-slots ((x list) (y list)) )
```



