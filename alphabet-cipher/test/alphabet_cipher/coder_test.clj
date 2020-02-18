(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer [deftest testing is]]
            [alphabet-cipher.coder :as cipher]))

(defn list->str [list]
  (apply str list))

(deftest test-rotate-alpha
  (testing "can rotate-alpha based on input ch"
    (is (= "abcdefghijklmnopqrstuvwxyz"
           (list->str (cipher/rotate-alpha \a))))
    (is (= "zabcdefghijklmnopqrstuvwxy"
           (list->str (cipher/rotate-alpha \z))))
    (is (= "qrstuvwxyzabcdefghijklmnop"
           (list->str (cipher/rotate-alpha \q))))))

(deftest test-letter-by-chart
  (testing "can obtain correct letter via chart"
    (is (= \a
           (cipher/letter-by-chart \a \a)))
    (is (= \y
           (cipher/letter-by-chart \z \z)))
    (is (= \g
           (cipher/letter-by-chart \z \h)))
    (is (= \t
           (cipher/letter-by-chart \e \p)))))

(deftest test-encode
  (testing "can encode a message with a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (cipher/encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (cipher/encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode a message given an encoded message and a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (cipher/decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (cipher/decode "scones" "egsgqwtahuiljgs")))))

; (deftest test-decipher
;   (testing "can extract the secret keyword given an encrypted message and the original message"
;     (is (= "vigilance"
;            (cipher/decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
;     (is (= "scones"
;            (cipher/decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))
;     (is (= "abcabcx"
;            (cipher/decipher "hfnlphoontutufa" "hellofromrussia")))))
