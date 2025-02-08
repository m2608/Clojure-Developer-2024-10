(ns otus-10.homework-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [otus-10.homework :as sut])
  (:import [java.io UnsupportedEncodingException]
           [java.io ByteArrayInputStream]))

(deftest test-unpack-be
  (testing "Распаковка тупоконечных значений"
    (are [value bytes-list] (= value (sut/unpack-be bytes-list))
         16r00000055 [16r55]
         16r00000102 [16r01 16r02]
         16r01020304 [16r01 16r02 16r03 16r04]
         16rFFFFFFFF [16rFF 16rFF 16rFF 16rFF])))

(deftest test-unpack-flags
  (testing "Распаковка флагов из байта"
    (are [value flags] (= value (sut/unpack-flags flags))
         [false false false false false false false false] 0
         [ true false  true false false  true false  true] 16rA5
         [ true  true  true  true  true  true  true  true] 16rFF)))

(deftest test-unpack
  (testing "Распаковка последовательности байт"
    (are [unpacked bytes-list bytes-format] (= unpacked (sut/unpack bytes-list bytes-format))
         ["ABC"]               [0x41 0x42 0x43]          [[:s 3]]            
         [[\A \B \C]]          [0x41 0x42 0x43]          [[:c 3]]            
         [\A \B \C]            [0x41 0x42 0x43]          [:c :c :c]          
         [2 3 16r0405 16r0607] [0 1 2 3 4 5 6 7]         [:x :x :b :b :w :d] 
         [16r02030405 16r0607] [0 1 2 3 4 5 6 7]         [[:x 2] :d :d]
         [[true false true false false true false true]] [16rA5] [:f])))

(deftest test-synchsafe
  (testing "Synchsafe значения"
    (are [value bytes-list] (= value (sut/synchsafe bytes-list))
         16r00     [16r00]
         16r7f     [16rFF]
         16r3fff   [16rFF 16rFF]
         16r3fff   [16r7F 16r7F]
         16r155555 [16r55 16r2a 16r55])))

(deftest test-parse-header
  (testing "Парсинг заголовка"
    (is (= ['(1 2 3)
            {:header
             {:magic "ID3",
              :version 4,
              :revision 0,
              :size 16r155555,
              :flags
              {:unsynchronization true,
               :extended-header false,
               :experimental-indicator false,
               :footer-present false}}}]
           (sut/parse-header [[16r49 16r44 16r33 16r04 16r00 16r80 16r00 16r55 16r2a 16r55 16r01 16r02 16r03] {}])))
    (is (thrown-with-msg? Exception #"^No ID3 tag found[.]$"
                          (sut/parse-header [[16r20 16r20 16r20 16r04 16r00 16r80 16r00 16r55 16r2a 16r55] {}])))
    (is (thrown-with-msg? Exception #"^Unknown tag version: 3$"
                          (sut/parse-header [[16r49 16r44 16r33 16r03 16r00 16r80 16r00 16r55 16r2a 16r55] {}])))
    (is (thrown-with-msg? Exception #"^Not enough data to unpack[.]$"
                          (sut/parse-header [[16r49 16r44 16r33 16r04 16r00 16r80] {}])))))

(deftest test-decode-bytes
  (testing "Чтение строки в различных кодировках"
    (are [value data encoding] (= value (sut/decode-bytes data encoding))
         "TestTEST 0123" (.getBytes "TestTEST 0123" "ISO-8859-1") "ISO-8859-1"
         "Проверка 0123" (.getBytes "Проверка 0123" "UTF-16")     "UTF-16"
         "Проверка 0123" (.getBytes "Проверка 0123" "UTF-16BE")   "UTF-16BE"
         "Проверка 0123" (.getBytes "Проверка 0123" "UTF-8")      "UTF-8")
    (is (thrown? UnsupportedEncodingException (sut/decode-bytes [] "XXX")))))

(deftest test-parse-with-encoding
  (testing "Декодирование текстовых фрагментов фрейма"
    (are [value codepage-id data] (= value (sut/parse-with-encoding codepage-id data))
         "TestTEST" 0 (.getBytes "TestTEST" "ISO-8859-1")
         "Проверка" 1 (.getBytes "Проверка" "UTF-16")
         "Проверка" 2 (.getBytes "Проверка" "UTF-16BE")
         "Проверка" 3 (.getBytes "Проверка" "UTF-8"))
    (is (thrown-with-msg? Exception #"^Unknown codepage id: 999[.]$"
                          (sut/parse-with-encoding 999 [0 1 2 3])))))

(deftest test-parse-text
  (testing "Декодирование текстовых фрагментов фрейма"
    (are [value data] (= value (sut/parse-text data))
         "Test 012" (cons 0 (.getBytes "Test 012" "ISO-8859-1"))
         "Проверка" (cons 1 (.getBytes "Проверка" "UTF-16"))
         "Проверка" (cons 2 (.getBytes "Проверка" "UTF-16BE"))
         "Проверка" (cons 3 (.getBytes "Проверка" "UTF-8")))
    (is (thrown-with-msg? Exception #"^Unknown codepage id: 999[.]$"
                          (sut/parse-text [999 0 1 2 3])))))

(deftest test-parse-frame-data
  (testing "Декодирование данных фреймов"
    (are [result id data] (= result (sut/parse-frame-data id data))
         ["Альбом" "Название альбома"] "TALB" (cons 3 (.getBytes "Название альбома"))
         ["Исполнитель" "Артист"]      "TPE1" (cons 3 (.getBytes "Артист"))
         ["Трек" "Название трека"]     "TIT2" (cons 3 (.getBytes "Название трека"))
         ["Год выпуска" "1970"]        "TYER" (cons 3 (.getBytes "1970"))
         ["Жанр" "Название жанра"]     "TCON" (cons 3 (.getBytes "Название жанра"))
         ["Издатель" "Паблишер"]       "TPUB" (cons 3 (.getBytes "Паблишер"))
         ["Комментарий (ENG)" "Test"]  "COMM" (concat [3] (.getBytes "ENG") (.getBytes "Test"))
         ["Изображение" "128 байт(а)"] "APIC" (range 128)

         ["Комментарий (RUS)" "Кратко Подробно"]
         "COMM" (concat '(3) (.getBytes "RUS") (.getBytes "Кратко") '(0) (.getBytes "Подробно"))

         ["Неизвестный фрейм (TAAA)" "Текст"]       "TAAA" (cons 3 (.getBytes "Текст"))
         ["Неизвестный фрейм (TXXX)" "00 01 02 03"] "TXXX" [0 1 2 3] 
         ["Неизвестный фрейм (XXXX)" "00 01 02 03"] "XXXX" [0 1 2 3])))

(deftest test-parse-frame
  (testing "Декодирование одного фрейма"
    (are [result data] (= result (sut/parse-frame [data {}]))
         ['()
          {:frames
           '({:id "TALB",
              :size 32,
              :flags-status
              {:tag-alter-preservation false,
               :file-alter-preservation false,
               :read-only true},
              :flags-format
              {:grouping-identity false,
               :compression false,
               :encryption false,
               :unsynchronization false,
               :data-length-indicator false},
              :data ["Альбом" "Название альбома"]})}]
         (concat (.getBytes "TALB") '(0 0 0 32) '(2r00010000 0) '(3) (.getBytes "Название альбома")))))

(deftest test-parse-frames
  (testing "Декодирование фреймов"
    (are [result data header] (= result (sut/parse-frames [data {:header header}]))
         ['()
          {:header {:size 70},
           :frames
           [{:id "TALB",
             :size 32,
             :flags-status
             {:tag-alter-preservation false,
              :file-alter-preservation false,
              :read-only true},
             :flags-format
             {:grouping-identity false,
              :compression false,
              :encryption false,
              :unsynchronization false,
              :data-length-indicator false},
             :data ["Альбом" "Название альбома"]}
            {:id "TIT2",
             :size 28,
             :flags-status
             {:tag-alter-preservation false,
              :file-alter-preservation false,
              :read-only false},
             :flags-format
             {:grouping-identity false,
              :compression false,
              :encryption false,
              :unsynchronization false,
              :data-length-indicator false},
             :data ["Трек" "Название трека"]}]}]
         (concat (.getBytes "TALB") '(0 0 0 32) '(2r00010000 0) '(3) (.getBytes "Название альбома")
                 (.getBytes "TIT2") '(0 0 0 28) '(0 0)          '(3) (.getBytes "Название трека"))
         {:size 70})))

(deftest test-byte-seq
  (testing "Чтение из потока"
    (are [result input] (= result (sut/byte-seq (ByteArrayInputStream. (.getBytes input))))
         (range 48 58) "0123456789")))

(deftest parse-file
  (testing "Парсинг файла"
    (is (thrown-with-msg? Exception #"^File not found: /nonexistent$" (sut/parse-file "/nonexistent")))
    (is (thrown-with-msg? Exception #"^Unknown tag version: 3$" (sut/parse-file (io/resource "test_230.mp3"))))
    (let [parsed (sut/parse-file (io/resource "test_240.mp3"))]
      (is (= (get-in parsed [:header :size]) 165126))
      (is (= (count (:frames parsed)) 10))
      (is (= (get-in parsed [:frames 3 :data 1]) "Radiohead")))))
