(ns alphabet-cipher.coder)

(defn char-pos [^Character chr]
  "Gives character offset relative to 'a'"
  (- (int chr) (int \a)))

(defn ^Character code-table-cell
  [^Character row-char ^Character col-char]
  "Finds a letter in a cell given a pair of row and column chars"
  (as-> [row-char col-char] $
    (map char-pos $)
    (apply + $)
    (-> $
        (mod 26)
        (+ (int \a))
        char)))

(defn rotating-keyword [kw]
  "Rotates chars of the keyword infinitely"
  (cycle (char-array kw)))

(defn encode [keyword message]
  "encodeme"
  (->> keyword
       rotating-keyword
       (map code-table-cell message)
       (apply str)))

(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")

