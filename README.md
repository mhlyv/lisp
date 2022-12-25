# Simple lisp interpreter

```scheme
$ cargo run
(define f (lambda (x) 1))
nil

(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))
nil

(fact 10)
3628800

(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))
nil

(fib 10)
55

(define map
  (lambda (f list)
    (if (= list '())
        '()
        (cons (f (car list))
              (map f (cdr list))))))
nil

(map (lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)

(define filter
  (lambda (f list)
    (if (= list '())
        '()
        (if (f (car list))
            (cons (car list) (filter f (cdr list)))
            (filter f (cdr list))))))
nil

(define mod (lambda (n m) (- n (* m (/ n m)))))
nil

(filter (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5 6 7))
(2 4 6)

(define reduce
  (lambda (f list)
    (if (= (cdr list) '())
        (car list)
        (f (car list) (reduce f (cdr list))))))
nil

(reduce + '(1 2 3))
6

(define compose (lambda (f g) (lambda (x) (f x))))
nil

((compose car cdr) '(1 2 3 4))
1
```
