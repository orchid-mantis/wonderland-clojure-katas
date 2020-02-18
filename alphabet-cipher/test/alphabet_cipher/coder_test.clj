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

(deftest test-letter-encode
  (testing "can encode keyword letter and message letter into single letter"
    (is (= \a
           (cipher/letter-encode \a \a)))
    (is (= \y
           (cipher/letter-encode \z \z)))
    (is (= \t
           (cipher/letter-encode \e \p)))))

(deftest test-letter-decode
  (testing "can encode keyword letter and message letter into single letter"
    (is (= \a
           (cipher/letter-decode \a \a)))
    (is (= \z
         (cipher/letter-decode \z \y)))
    (is (= \z
           (cipher/letter-decode \s \r)))))

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
