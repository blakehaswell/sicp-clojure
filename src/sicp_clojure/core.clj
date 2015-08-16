(ns sicp-clojure.core)

(defn fib1
  "Computes the nth number in the Fibonacci sequence. Uses a linear
  recursive algorithm. Probably the worst possible way to code this,
  but it reads well."
  [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib1 (- n 1))
             (fib1 (- n 2)))))

(defn fib2
  "Computes the nth number in the Fibonacci sequence. Uses an
  iterative algothm, which has much better performance with larger
  values of n."
  [n]
  (letfn [(f [a b count]
            (if (= count 0)
              b
              (f (+ a b) a (- count 1))))]
    (f 1 0 n)))

(defn fib3
  "Computes the nth number in the Fibonacci sequence. Uses a clever
  algorithm which computers in logarithmic time."
  [n]
  (letfn [(f [a b p q count]
            (cond
              (= count 0) b
              (even? count) (f a
                               b
                               (+ (* p p) (* q q))
                               (+ (* 2 p q) (* q q))
                               (/ count 2))
              :else (f (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (- count 1))))]
    (f 1 0 0 1 n)))
