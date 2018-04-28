(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

(defn- clamp-pos
  "Moves given `pos` to a 0-26 range"
  [pos]
  (loop [p pos]
    (if (>= p 0)
      (mod p 26)
      (recur (+ p 26)))))

(defn- char->pos
  "Given a character `chr`, returns its offset relative to \\a"
  [^Character chr]
  (- (int chr) (int \a)))

(defn- pos->char
  "Given some offset `pos` relative to \\a turns it into actual char"
  [^Integer pos]
  (char (+ (clamp-pos pos) (int \a))))

(defn- letter+
  "Sums its args thinking they are English alphabet letters"
  [& args]
  (pos->char (apply + (map char->pos args))))

(defn- letter-
  "Subtracts its args thinking they are English alphabet letters"
  [& args]
  (pos->char (apply - (map char->pos args))))

(defn- ^Character code-table-cell
  "Finds a letter in a cell given a pair of row and column chars"
  [^Character row-char ^Character col-char]
  (letter+ row-char col-char))

(defn- ^Character code-table-row
  "Finds a letter in a row given a pair of cell and column chars"
  [^Character cell-char ^Character col-char]
  (letter- cell-char col-char))

(defn- ^Character code-table-column
  "Finds a letter in a column given a pair of cell and row chars"
  [^Character cell-char ^Character row-char]
  (letter- cell-char row-char))

(defn- rotating
  "Rotates chars of the keyword `kw` infinitely"
  [kw]
  (cycle (lazy-seq kw)))

(defn- kw-boundary?
  "If given `coll` consists of rotating fragments of length `n`,
   returns that `n`"
  [coll n]
  (let [chunks (map (partial apply str) (partition-all n coll))]
    (when (and
           (apply = (butlast chunks))
           (str/starts-with? (first chunks) (last chunks)))
      n)))

(defn- not-rotating
  "Given a rotating keyword `kw` converts it back to normal word"
  [kw]
  (let [possible-positions (range 1 (quot (count kw) 2))
        kw-boundary (some (partial kw-boundary? kw) possible-positions)]
    (when kw-boundary (subs kw 0 kw-boundary))))

(defn encode
  "encodeme"
  [keyword message]
  (->> (rotating keyword)
       (map code-table-cell message)
       (apply str)))

(defn decode
  "decodeme"
  [keyword message]
  (->> (rotating keyword)
       (map code-table-row message)
       (apply str)))

(defn decipher
  "decypherme"
  [cipher message]
  {:pre [(= (count cipher) (count message))]}
  (->> message
       (map code-table-column cipher)
       (apply str)
       not-rotating))

