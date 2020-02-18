(ns alphabet-cipher.coder)

(def alphabet '(\a \b \c \d \e \f \g \h \i \j \k \l \m
                   \n \o \p \q \r \s \t \u \v \w \x \y \z))

(defn rotate-alpha
  "Rotate alphabet until given char is first char of seq"
  [ch]
  (let [split-by-char (partition-by #(= ch %) alphabet)
        [left mid right] split-by-char]
    (cond
      (= ch \a) alphabet
      (= ch \z) (concat mid left)
      :else (concat mid right left))))

(defn letter-by-chart [key-letter msg-letter]
  (let [row (rotate-alpha msg-letter)
        indices (zipmap alphabet (range))
        column (get indices key-letter)]
    (nth row column)))

(defn encode [keyword message]
  (let [msg-size (count message)
        repeat-keyword (apply str (take msg-size (cycle keyword)))]
    (->> (map letter-by-chart repeat-keyword message)
         (apply str))))

(defn letter-decode [key-letter msg-letter]
  (let [row (rotate-alpha key-letter)
        indices (zipmap row (range))
        column (get indices msg-letter)]
    (nth alphabet column)))

(defn decode [keyword message]
  (let [msg-size (count message)
        repeat-keyword (apply str (take msg-size (cycle keyword)))]
    (->> (map letter-decode repeat-keyword message)
         (apply str))))

(defn decipher [cipher message]
  "decypherme")

