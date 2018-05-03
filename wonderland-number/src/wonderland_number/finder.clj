(ns wonderland-number.finder)

(letfn [(digit-iter [n]
          (lazy-seq
           (when (pos? n)
             (cons (mod n 10) (digit-iter (quot n 10))))))
        (digits [n] (sort (digit-iter n)))]
  (defn- wondernum? [n]
    (every? (partial = (digits n))
            (->> (range 2 7)
                 (map (partial * n))
                 (map digits))))
  (defn- sum-of-cubes? [n]
    (= n (apply + (map #(* % % %) (digits n))))))

(defn- match-number [criterion]
  #(when (criterion %) %))

(defn- special-number [criterion range]
  (some (match-number criterion) range))

(defn wonderland-number []
  (special-number wondernum? (range 100000 1000000)))

(defn self-cube-number []
  (special-number sum-of-cubes? (range 100 1000)))
