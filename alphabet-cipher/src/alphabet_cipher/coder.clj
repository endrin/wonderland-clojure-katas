(ns alphabet-cipher.coder)

(defn char-pos
  "Given a character `chr`, returns its offset relative to \\a"
  [^Character chr]
  (- (int chr) (int \a)))

(defn ^Character code-table-cell
  "Finds a letter in a cell given a pair of row and column chars"
  [^Character row-char ^Character col-char]
  (as-> [row-char col-char] $
    (map char-pos $)
    (apply + $)
    (-> $
        (mod 26)
        (+ (int \a))
        char)))

(defn rotating-keyword
  "Rotates chars of the keyword infinitely"
  [kw]
  (cycle (seq kw)))

(defn encode
  "encodeme"
  [keyword message]
  (->> keyword
       rotating-keyword
       (map code-table-cell message)
       (apply str)))

(defn decode
  "decodeme"
  [keyword message])

(defn decipher
  "decypherme"
  [cipher message])

