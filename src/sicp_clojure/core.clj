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
  (loop [a 1
         b 0
         count n]
    (if (= count 0)
      b
      (recur (+' a b) a (- count 1)))))

(defn fib3
  "Computes the nth number in the Fibonacci sequence. Uses a clever
  algorithm which computers in logarithmic time."
  [n]
  (loop [a 1
         b 0
         p 0
         q 1
         count n]
    (cond
      (= count 0) b
      (even? count) (recur a
                           b
                           (+' (*' p p) (*' q q))
                           (+' (*' 2 p q) (*' q q))
                           (/ count 2))
      :else (recur (+' (*' b q) (*' a q) (*' a p))
                   (+' (*' b p) (*' a q))
                   p
                   q
                   (- count 1)))))

(defn expt1
  "Computes the exponential (n) of a given number (b)."
  [b n]
  (if (= n 0)
    1
    (*' b (expt1 b (- n 1)))))

(defn expt2
  "Computes the exponential (n) of a given number (b)"
  [b n]
  (loop [count n
         product 1]
    (if (= count 0)
      product
      (recur (- count 1) (*' b product)))))

(defn expt3
  "Computes the exponential (n) of a given number (b)"
  [b n]
  (cond
    (= n 0) 1
    (even? n) (expt3 (*' b b) (/ n 2))
    :else (* b (expt3 b (- n 1)))))

(defn expt4
  "Computes the exponential (n) of a given number (b)"
  [b n]
  (loop [b b
         n n
         a 1]
    (cond
      (= n 0) a
      (even? n) (recur (*' b b) (/ n 2) a)
      :else (recur b (- n 1) (*' b a)))))
