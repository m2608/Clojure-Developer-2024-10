(ns otus-10.homework
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn unpack-be
  "Распаковываем тупоконечное значение, переданное в виде последовательности байт."
  [vals]
  (let [n (count vals)]
    (reduce + (map-indexed (fn [i v] (bit-shift-left v (* (- n i 1) 8)))
                           vals))))

(defn unpack-flags
  "Распаковывает биты переданного байта в массив булевых значений, начиная от старшего
  к младшему."
  [b]
  (mapv (fn [n]
          (pos? (bit-and b (bit-shift-left 1 n))))
        (range 7 -1 -1)))

;; Размеры распаковываемых элементов в байтах.
(def unpack-format-size
  {:x 1 ; пропускаем один байт
   :b 1 ; беззнаковый байт
   :w 2 ; беззнаковое слово
   :d 4 ; беззнаковое двойное слово
   :f 1 ; байт с флагами
   :c 1 ; байт, преобразованный в символ
   :s 1 ; аналогично :c, но при распаковке массива формируется строка
   })

(defn unpack
  "Простенькая функция для распаковки значений из последовательности байт. Примеры:

  `:s` распаковывает строку указанного размера.
  
  `(unpack [0x41 0x42 0x43] [[:s 3]])             ; [\"ABC\"]`

  `:c` распаковываем символ или вектор символов.

  `(unpack [0x41 0x42 0x43] [[:c 3]])             ; [[\\A \\B \\C]]`
  `(unpack [0x41 0x42 0x43] [:c :c :c])           ; [\\A \\B \\C]`

  `:x` пропускает байт.

  `(unpack [0 1 2 3 4 5 6 7] [:x :x :b :b :w :d]) ; [2 3 1029 1543]`
  `(unpack [0 1 2 3 4 5 6 7] [[:x 2] :d :d])      ; [33752069 1543]`
  "
  [bytes format]
  (loop [bs bytes fs format result []]
    (cond
      (empty? fs)
      result

      (empty? bs)
      (throw (Exception. "Not enough data to unpack."))
      
      ;; Обрабатываем вариант, когда в качестве формата указан вектор. В этом случае
      ;; распаковка байт также производится в вектор (указанной длины). Для строк
      ;; (eg. `[:s 10]`) распаковка выполняется не в вектор символов, а в строку.
      (vector? (first fs))
      (let [[el n] (first fs)
            sz (* n (unpack-format-size el))]
        (recur (drop sz bs)
               (rest fs)
               (if (= el :x) result
                 (conj result ((if (= el :s) (partial apply str) identity)
                               (unpack (take sz bs) (vec (repeat n el))))))))

      ;; Обрабатываем специферы.
      :else
      (let [el (first fs)
            sz (unpack-format-size el)]
        (recur (drop sz bs)
               (rest fs)
               (if (= el :x) result
                 (conj result
                       (case el
                         (:b :w :d) (unpack-be (take sz bs))
                         (:c :s) (char (first bs))
                         :f (unpack-flags (first bs))
                         (throw (Exception. (format "Unknown format specifier: \"%s\"." (str el))))))))))))


(defn synchsafe
  "Вычисляет т.н. synchsafe значения. Особенность таких значений в том, что 7й бит выкидывается,
  а остальные выстраиваются в цепочку. Количество байт может быть произвольным."
  [bs]
  (let [length (count bs)]
    (->> bs
         (map-indexed
           (fn [i b]
             (bit-shift-left (bit-and b 16r7f)
                             (* 7 (- length i 1)))))
         (reduce +))))

;; Функции парсинга (`parse-...`) устроены следующим образом:
;;
;; - Принимают один аргумент из двух элементов:
;;   - Последовательность байт для парсинга.
;;   - Мапа, содержащая разобранные на данный момент поля.
;; - Возвращает такую же пару:
;;   - Последовательность байт, из которой дропнуты распаршенные функцией байты.
;;   - Мапа, обогащенная новыми разобранными полями.
;; 
;; Таким образом функции можно выстраивать в цепочки, на каждом этапе последовательность
;; байт будет сокращаться, а мапа будет обогащаться новыми полями.

(defn parse-header
  "Парсит основной заголовок id3v2 тега."
  [[data parsed]]
  (let [;; Размер заголовка.
        length 10
        ;; Поля заголовка.
        [magic [version revision] [unsync extended experimental footer] size]
        (unpack (take length data) [[:s 3] [:b 2] :f [:b 4]])]
    (cond
      (not= magic "ID3")
      (throw (Exception. "No ID3 tag found."))

      (not= version 4)
      (throw (Exception. (format "Unknown tag version: %d" version)))

      :else
      [(drop length data)
       (merge parsed {:header {:magic magic
                               :version version
                               :revision revision
                               :size (synchsafe size)
                               :flags {:unsynchronization unsync
                                       :extended-header extended
                                       :experimental-indicator experimental
                                       :footer-present footer}}})])))


(defn parse-ext-header
  "Парсит обязательные поля расширенного заголовка. Остальные поля игнорирует,
  всё равно расширенный заголовок нигде не поддерживается."
  [[data parsed]]
  (if (get-in parsed [:header :flags :extended-header])
    (let [;; Размер (4 байта) + размер флагов (1 байт).
          length 5
          ;; Распаковываем.
          [size flag-bytes] (unpack (take length data) [[:b 4] :b])
          synchsafe-size (synchsafe size)]
      [(drop synchsafe-size data)
       (merge parsed {:extended-header {:size synchsafe-size
                                        :flag-bytes flag-bytes}})])
    [data parsed]))

(defn decode-bytes
  "Преобразует список байт в тест в указанной кодировке."
  [data encoding]
  (String. (byte-array (count data) data) encoding))

(defn parse-with-encoding
  "Преобразует список байт в одну из кодовых страниц, заданных
  числовыми идентификаторами."
  [codepage-id data]
  (decode-bytes
    data
    (case codepage-id
      0 "ISO-8859-1"
      1 "UTF-16"
      2 "UTF-16BE"
      3 "UTF-8"
      (throw (Exception. (format "Unknown codepage id: %d." codepage-id))))))

(defn parse-text
  "Парсит текстовые данные фрейма в одной из кодировок."
  [data]
  (parse-with-encoding (first data) (rest data)))

(defmulti parse-frame-data
  (fn [id _data] id))

(defmethod parse-frame-data "TALB"
  [_id data]
  ["Альбом" (parse-text data)])

(defmethod parse-frame-data "TPE1"
  [_id data]
  ["Исполнитель" (parse-text data)])

(defmethod parse-frame-data "TIT2"
  [_id data]
  ["Трек" (parse-text data)])

(defmethod parse-frame-data "TYER"
  [_id data]
  ["Год выпуска" (parse-text data)])

(defmethod parse-frame-data "TCON"
  [_id data]
  ["Жанр" (parse-text data)])

(defmethod parse-frame-data "TPUB"
  [_id data]
  ["Издатель" (parse-text data)])

(defmethod parse-frame-data "COMM"
  [_id data]
  (let [[cp-id lang] (unpack (take 4 data) [:b [:s 3]])
        text (vec (drop 4 data))
        zero (.indexOf text 0)]
    [(str "Комментарий (" lang ")")
     (str/join " " (map (partial parse-with-encoding cp-id)
                        (if (neg? zero) (list text)
                          (list (subvec text 0 zero) (subvec text (inc zero))))))]))

(defmethod parse-frame-data "APIC"
  [_id data]
  ["Изображение" (format "%d байт(а)" (count data))])

(defmethod parse-frame-data :default
  [id data]
  [(format "Неизвестный фрейм (%s)" id)
       ;; Текстовые теги начинаются с буквы "T". Тег "TXXX" парсится немного
       ;; иначе, пропускаем его.
       (if (and (str/starts-with? id "T")
                (not= id "TXXX"))
         ;; Для текстовых тегов печатаем текст.
         (parse-text data)
         ;; Для остальных список байтов.
         #_(format "%d byte(s)" (count data))
         (str/join " " (map (partial format "%02x") data)))])

(defn parse-frame
  "Парсит фрейм. Возвращает `nil`, если фрейм невалидный."
  [[data parsed]]
  (let [length 10
        ;; Сначала распаковываем заголовок фрейма: идентификатор, размер и два байта с флагами.
        [id size
         [_ tag-alter-preservation file-alter-preservation read-only]
         [_ grouping-identity _ _ compression encryption unsynchronization data-length-indicator]]
        (unpack (take length data) [[:s 4] [:b 4] :f :f])
        synchsafe-size (synchsafe size)]
    ;; Если заголовок представляется валидным, распаковываем данные фрейма. Иначе возвращаем
    ;; `nil`. На этом распаковка тега закончится.
    (when (re-matches #"^[A-Z0-9]{4}$" id) 
      (let [frame-data (vec (take synchsafe-size (drop length data)))]
        [(drop (+ length synchsafe-size) data)
         (update parsed :frames #(conj % {:id id
                                          :size synchsafe-size
                                          :flags-status {:tag-alter-preservation tag-alter-preservation
                                                         :file-alter-preservation file-alter-preservation
                                                         :read-only read-only}
                                          :flags-format {:grouping-identity grouping-identity
                                                         :compression compression
                                                         :encryption encryption
                                                         :unsynchronization unsynchronization
                                                         :data-length-indicator data-length-indicator}
                                          :data (parse-frame-data id frame-data)}))]))))

(defn parse-frames
  "Парсит фреймы, пока функция парсинга фрейма не вернет `nil`."
  [[data parsed]]
  (loop [dp [data (assoc parsed :frames [])]
         rest-header-size (- (get-in parsed [:header :size]) 10)]
    ;; Если находимся в пределах заголовка - продолжаем парсинг фреймов.
    (if (pos? rest-header-size)
      ;; Если получили валидный фрейм - продолжаем парсинг фреймов.
      (if-let [[_ p :as dp'] (parse-frame dp)]
        (recur dp' (- rest-header-size (:size (peek (:frames p)))))
        dp)
      dp)))

(defn byte-seq
  "Возвращает ленивую последовательность байт для указанного потока ввода."
  [in]
  (lazy-seq
    (let [b (.read in)]
      (when (nat-int? b)
        (cons b (byte-seq in))))))

(defn print-frames
  "Печатает распаршенные фреймы."
  [parsed-data]
  (println (str/join "\n" (map #(str/join ": " (:data %)) (:frames parsed-data)))))

(defn parse-file
  "Парсит указанный файл. Возвращает словарь с разобранными данными."
  [filename]
  (if (.exists (io/file filename))
    (with-open [in (io/input-stream filename)]
      (-> [(byte-seq in) {}]
          parse-header
          parse-ext-header
          parse-frames
          second))
    (throw (Exception. (format "File not found: %s" filename)))))

(defn -main
  [& args]
  (if-let [filename (first args)]
    (try
      (print-frames (parse-file filename))
      (catch Exception e
        (println (.getMessage e))))
    (println "Usage: lein run <mp3 file>")))
