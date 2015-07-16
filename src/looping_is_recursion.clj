(ns looping-is-recursion)

(defn power [base exp]
  (loop [b base e exp x 1]
    (if (zero? e) x
        (recur b (dec e) (* x b)))))

(defn last-element [a-seq]
  (cond (empty? a-seq) nil
        (= (count a-seq) 1) (first a-seq)
        :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (not= (count seq1) (count seq2)) false
        (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
        :else false))

(defn find-first-index [pred a-seq]
  (loop [p pred v a-seq i 0]
    (cond (empty? v) nil
          (pred (first v)) i
          :else (recur p (rest v) (inc i)))))

(defn avg [a-seq]
  (let [n (count a-seq)]
    (loop [v a-seq sum 0]
      (if (empty? v) (/ sum n)
          (recur (rest v) (+ sum (first v)))))))

(defn toggle [s e]
  (if (contains? s e) (disj s e)
      (conj s e)))

(defn parity [a-seq]
  (loop [v a-seq s #{}]
    (if (empty? v) s
        (recur (rest v) (toggle s (first v))))))

(defn fast-fibo [n]
  (loop [nth n n 0 n2 1]
    (if (zero? nth) n
        (recur (dec nth) n2 (+ n n2)))))

(defn cut-at-repetition [a-seq]
  (loop [v a-seq out [] s #{}]
    (let [f (fn [coll] (conj coll (first v)))]
      (cond (empty? v) out
            (contains? s (first v)) out
            :else (recur (rest v) (f out) (f s))))))
