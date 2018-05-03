(ns wonderland-number.finder)

(letfn [(digit-iter [n]
          (lazy-seq
           (when (pos? n)
             (cons (mod n 10) (digit-iter (quot n 10))))))
        (digits [n] (sort (digit-iter n)))]
  (defn wondernum? [n]
    (when
     (every? (partial = (digits n))
             (->> (range 2 7)
                  (map (partial * n))
                  (map digits)))
      n)))

(defn wonderland-number []
  (some wondernum? (range 100000 1000000)))
