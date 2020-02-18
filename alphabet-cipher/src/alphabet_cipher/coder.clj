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

(defn letter-by-chart [msg-char index-from column-from]
  (let [indices (zipmap index-from (range))
        column (get indices msg-char)]
    (nth column-from column)))

(defn letter-encode [key-char msg-char]
  (letter-by-chart msg-char alphabet (rotate-alpha key-char)))

(defn letter-decode [key-char msg-char]
  (letter-by-chart msg-char (rotate-alpha key-char) alphabet))

(defn cipher [f keyword message]
  (let [msg-size (count message)
        repeat-keyword (apply str (take msg-size (cycle keyword)))]
    (->> (map f repeat-keyword message)
         (apply str))))

(defn encode [keyword message]
  (cipher letter-encode keyword message))

(defn decode [keyword message]
  (cipher letter-decode keyword message))

(defn decipher [cipher message]
  "decypherme")

