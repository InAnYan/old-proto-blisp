(global pi 3.14)
(global should-not-unintialized-global-var 5)

(function fib (n)
          (if (== n 1)
              (return 1)
              (return (* n (fib (- n 1))))))

(function some-calculations (x y)
          (local a (+ x y))
          (local b (* x a))
          (assign b (+ b a))
          (return b))

(function main ()
          (local i (input-int))
          (while (!= i 0)
                 (print-int (fib i))
                 (assign i (- i 1))))
