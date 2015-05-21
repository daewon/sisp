sisp
====

simple lisp implementation in scala

# test
```bash
sbt test
```

# repl
```bash
sbt run
```

# example in run
```lisp
sisp> (define sum-list (lambda (xs) (if xs (+ (car xs) (sum-list (cdr xs))) 0)))
sum-list
sisp> (sum-list `(1 2 3))
6
sisp>


```
