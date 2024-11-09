(ns otus-02.homework.palindrome
  (:require [clojure.string :as s]))

(defn is-palindrome
  "Проверяет, является ли строка палиндромом."
  [^String test-string]
  (let [ts (-> test-string s/lower-case (s/replace #"[^\w]" ""))]
    (= ts (s/reverse ts))))

