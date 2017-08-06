(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [res n k]
              (if (= k 0)
                res
                (recur (* res n) n (dec k))))]
    (pow 1 base exp)))



(defn last-element [a-seq]
  (let [last-el (fn [elem b-seq]
               (if (empty? b-seq)
                 elem
                 (recur (first b-seq) (rest b-seq))))]
    (last-el (first a-seq) a-seq)))

(defn seq= [seq1 seq2]
  (let [s (fn [rest-seq1 rest-seq2]
            (if (not (= (count rest-seq1)(count rest-seq2)))
              false
              (if (and (empty? rest-seq1) (empty? rest-seq2))
                true
                (if (= (first rest-seq1) (first rest-seq2))
                  (recur (rest rest-seq1)(rest rest-seq2))
                  false))))]
    (s seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pre pred sq a-seq ind 0]
    (if (empty? sq)
      nil
      (if (pre (first sq))
        ind
        (recur pre (rest sq) (+ ind 1))))))

(defn avg [a-seq]
  (loop [sq a-seq sum 0 size (count a-seq)]
    (if (empty? sq)
      (/ sum size)
      (recur (rest sq) (+ sum (first sq)) size))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [sq a-seq ret-set (set [])]
    (if (empty? sq)
      ret-set
      (recur (rest sq) (toggle ret-set (first sq))))))

(defn fast-fibo [n]
  (cond
      (= n 0) 0
      (= n 1) 1
      :else
        (loop [number n i 2 f-1 1 f-2 0]
          (if (= number i)
            (+ f-1 f-2)
            (recur number (+ i 1)(+ f-1 f-2) f-1)))))

(defn cut-at-repetition [a-seq]
  (loop [sq a-seq test-set (set []) result (vec nil)]
    (if (or (contains? test-set (first sq)) (empty? sq))
      result
      (recur (rest sq) (conj test-set (first sq)) (conj result (first sq))))))

