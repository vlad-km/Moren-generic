# DAS- simple implementation of CommonLisp Generic Function for Moren Edition JSCL


## Simple implementation of 'defgeneric' and 'defmethod'

Macros: 
- das:generic 
- das:method

Looks like something this:

```lisp
   (das:generic msort (vector))
   (das:method msort ((object vector)) 
       (funcall ((oget object "sort" "bind") object)))

   (msort (vector 9 2 3 1 2 3 4))
   ;;=> #(1 2 2 3 3 4 9)

   (das:method msort ((object list)) 
        (vector-to-list (msort (list-to-vector object))))

   (msort '(1 2 3 9 8 1 2 3))
   ;;=> (1 1 2 2 3 3 8 9)

   (das:generic msort (vector &optional function))


   (das:method msort ((object vector) &optional comparator)  
         (funcall ((oget object "sort" "bind") object comparator)))

   (msort (vector 9 8 7 1 2))
   ;;=> #(1 2 7 8 9)

   (msort (vector #\a #\b 9 8 #\c 7 1 2))
   ;;=> #(1 2 7 8 9 #\a #\b #\c)

   (das:generic compare-slots (x y)) 

   (das:method compare-slots ((x integer)(y integer)) 
       (cond ((< x y) 1) 
             ((= x y) 0) 
             (t -1))) 

   (das:method compare-slots ((x list) (y list)) 
        (compare-slots (length x) (length y)))

   (msort (vector '(1 2 3) '(1) '(1 2) '(1 2 3)) #'compare-slots)
   ;;=> #((1 2 3) (1 2 3) (1 2) (1))
```

### das:generic

```lisp
   (das:generic gfname (args))
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
                       | das!structure-name 

```lisp
    (das:method compare-slots ((x integer) (y integer)) )
    (das:method compare-slots ((x integer) y ) )
    (das:method compare-slots ((x list) (y list)) )
```



