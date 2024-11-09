(ns otus-02.homework.pangram
  (:require [clojure.string :as s]))

(defn is-pangram
  "Проверяет, является ли строка панграммой для заданного алфавита. По
  умолчанию используется английский алфавит."

  ([test-string]
   (is-pangram test-string "abcdefghijklmnopqrstuvwxyz"))

  ([test-string alphabet]
   (every? (set (s/lower-case test-string)) alphabet)))
