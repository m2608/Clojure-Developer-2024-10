(ns otus-06.homework
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; Загрузить данные из трех файлов на диске.
;; Эти данные сформируют вашу базу данных о продажах.
;; Каждая таблица будет иметь «схему», которая указывает поля внутри.
;; Итак, ваша БД будет выглядеть так:

;; cust.txt: это данные для таблицы клиентов. Схема:
;; <custID, name, address, phoneNumber>

;; Примером файла cust.txt может быть:
;; 1|John Smith|123 Here Street|456-4567
;; 2|Sue Jones|43 Rose Court Street|345-7867
;; 3|Fan Yuhong|165 Happy Lane|345-4533

;; Каждое поле разделяется символом «|». и содержит непустую строку.

;; prod.txt: это данные для таблицы продуктов. Схема
;; <prodID, itemDescription, unitCost>

;; Примером файла prod.txt может быть:
;; 1|shoes|14.96
;; 2|milk|1.98
;; 3|jam|2.99
;; 4|gum|1.25
;; 5|eggs|2.98
;; 6|jacket|42.99

;; sales.txt: это данные для основной таблицы продаж. Схема:
;; <salesID, custID, prodID, itemCount>.
;;
;; Примером дискового файла sales.txt может быть:
;; 1|1|1|3
;; 2|2|2|3
;; 3|2|1|1
;; 4|3|3|4

;; Например, первая запись (salesID 1) указывает, что Джон Смит (покупатель 1) купил 3 пары обуви (товар 1).

;; Задача:
;; Предоставить следующее меню, позволяющее пользователю выполнять действия с данными:

;; *** Sales Menu ***
;; ------------------
;; 1. Display Customer Table
;; 2. Display Product Table
;; 3. Display Sales Table
;; 4. Total Sales for Customer
;; 5. Total Count for Product
;; 6. Exit

;; Enter an option?


;; Варианты будут работать следующим образом

;; 1. Вы увидите содержимое таблицы Customer. Вывод должен быть похож (не обязательно идентичен) на

;; 1: ["John Smith" "123 Here Street" "456-4567"]
;; 2: ["Sue Jones" "43 Rose Court Street" "345-7867"]
;; 3: ["Fan Yuhong" "165 Happy Lane" "345-4533"]

;; 2. То же самое для таблицы prod.

;; 3. Таблица продаж немного отличается.
;;    Значения идентификатора не очень полезны для целей просмотра,
;;    поэтому custID следует заменить именем клиента, а prodID — описанием продукта, как показано ниже:
;; 1: ["John Smith" "shoes" "3"]
;; 2: ["Sue Jones" "milk" "3"]
;; 3: ["Sue Jones" "shoes" "1"]
;; 4: ["Fan Yuhong" "jam" "4"]

;; 4. Для варианта 4 вы запросите у пользователя имя клиента.
;;    Затем вы определите общую стоимость покупок для этого клиента.
;;    Итак, для Сью Джонс вы бы отобразили такой результат:
;; Sue Jones: $20.90

;;    Это соответствует 1 паре обуви и 3 пакетам молока.
;;    Если клиент недействителен, вы можете либо указать это в сообщении, либо вернуть $0,00 за результат.

;; 5. Здесь мы делаем то же самое, за исключением того, что мы вычисляем количество продаж для данного продукта.
;;    Итак, для обуви у нас может быть:
;; Shoes: 4

;;    Это представляет три пары для Джона Смита и одну для Сью Джонс.
;;    Опять же, если продукт не найден, вы можете либо сгенерировать сообщение, либо просто вернуть 0.

;; 6. Наконец, если выбрана опция «Выход», программа завершится с сообщением «До свидания».
;;    В противном случае меню будет отображаться снова.


;; *** Дополнительно можно реализовать возможность добавлять новые записи в исходные файлы
;;     Например добавление нового пользователя, добавление новых товаров и новых данных о продажах


;; Файлы находятся в папке otus-06/resources/homework

; Файлы таблиц и соответствующие им описания схем.
(def db-description
  {:customers
   {:path "homework/cust.txt"
    :schema "<custID, name, address, phoneNumber>"}
   :products
   {:path "homework/prod.txt"
    :schema "<prodID, itemDescription, unitCost>"}
   :sales
   {:path "homework/sales.txt"
    :schema "<salesID, custID, prodID, itemCount>"}})

(defn parse-schema
  "Парсит схему таблицы, возвращает вектор из имён полей
  или `nil` в случае ошибки."
  [schema-line]
  (when-let [m (re-matches #"^[<](.+)[>]$" schema-line)]
    (mapv keyword (str/split (second m) #",[ ]*"))))

; Парсим описания схем для удобства.
(def db
  (->> db-description
       (map (fn [[table {path :path schema :schema}]]
              [table {:path path :schema (parse-schema schema)}]))
       (into {})))

; Разделитель столбцов.
(def delimiter #"[|]")

(defn autocoerce
  "Пробует привести значение последовательно к `Integer` и `BigDecimal`.
  Если не получается, возвращает значение как есть."
  [value]
  (try (Integer. value)
       (catch NumberFormatException _
         (try (BigDecimal. value)
              (catch NumberFormatException _ value)))))

(defn parse-row
  "Парсит строку по указанной схеме, возвращает мапу."
  [schema line]
  (zipmap schema (map autocoerce (str/split line delimiter))))

(defn symb-line
  "Возвращает строку, состоящую из `n` указанных символов."
  [s n]
  (apply str (repeat n s)))

; Возвращает строку дефисов.
(def dash-line (partial symb-line \-))

; Возвращает строку пробелов.
(def spaces (partial symb-line \space))

(defmulti pad
  "Выполняет паддинг значения в зависимости от его типа."
  (fn [value _] (class value)))

; Для чисел производится выравнивание по правому краю.
(defmethod pad java.lang.Integer
  [value padding]
  (format (str "%" padding "d") value))

(defmethod pad java.math.BigDecimal
  [value padding]
  (format (str "%" padding ".2f") value))

; Кейворды приводятся к строке и центрируются.
(defmethod pad clojure.lang.Keyword
  [value padding]
  (let [value (name value)
        full (- padding (count value))
        pref (quot full 2)
        post (- full pref)]
    (str (spaces pref) value (spaces post))))

; Для остальных типов значений - выравнивание по левому краю.
(defmethod pad :default
  [value padding]
  (str value (spaces (- padding (count (str value))))))

(defn table-row
  "Возвращает текстовое представление строки таблицы. Поля выводятся в
  порядке, заданным в векторе `schema`. Ширина каждого поля задается в мапе `widths`."
  [schema widths row]
  (str "| " (str/join " | " (map (fn [col] (pad (row col) (widths col))) schema)) " |"))

(defn table-header
  "Возвращает текстовое представление заголовка таблицы."
  [schema widths]
  (str "| " (str/join " | " (map #(pad % (widths %)) schema)) " |"))

(defn table-separator
  "Возвращает декоративную строку, отделяющую заголовок таблицы от её содержимого."
  [schema widths]
  (str "+ " (str/join " + " (map (comp dash-line widths) schema)) " +"))

(defn get-max-widths
  "Считает максимальную ширину для всех колонок из списка строк (включая заголовок).
  Возвращает мапу, где ключами будут названия колонок."
  [schema rows]
  (->> schema
       (map (fn [col]
              [col (apply max
                          (count (name col))
                          (map (comp count str col) rows))]))
       (into {})))

(defn select
  "Выбирает из таблицы строки, для которых фильтрующая функция вернёт истину."
  [db table-name filter-fn]
  (->> (db table-name)
       :path
       io/resource
       io/reader
       line-seq
       (map (partial parse-row (:schema (db table-name))))
       (filter filter-fn)))

(defn print-table
  "Красиво печатает указанную таблицу."
  [db table-name]
  (let [schema (:schema (db table-name))
        rows (select db table-name (constantly true))
        widths (get-max-widths schema rows)]
    (println (str "\n" (table-header    schema widths)
                  "\n" (table-separator schema widths)
                  "\n" (str/join "\n" (map (partial table-row schema widths) rows))))))

(defn print-sales-for-customer
  "Печатает сумму, потраченную указанным покупателем на все покупки."
  [db customer-name]
  (if-let [customer (first (select db :customers #(= (:name %) customer-name)))]
    (->> (select db :sales #(= (:custID %) (:custID customer)))
         (map (fn [sale]
                (let [product (first (select db :products #(= (:prodID %) (:prodID sale))))]
                  (* (:unitCost product) (:itemCount sale)))))
         (reduce +)
         (format "\n%s: $%.2f" customer-name)
         println)
    (println (format "\nCustomer \"%s\" not found." customer-name))))

(defn print-sales-for-product
  "Печатает количество проданных единиц указанного продукта."
  [db product-name]
  (if-let [product (first (select db :products #(= (:itemDescription %) product-name)))]
    (->> (select db :sales #(= (:prodID %) (:prodID product)))
         (map :itemCount)
         (reduce +)
         (format "\n%s: %d" product-name)
         println)
    (println (format "\nProduct \"%s\" not found." product-name))))

(defn prompt
  "Выводит приглашение и ожидает ввода пользователя."
  [p]
  (print p)
  (flush)
  (read-line))

(defn get-menu
  "Возвращает меню."
  [db]
  {:title "*** Sales Menu ***"
   :exit "Exit"
   :message "Enter option: "
   :error "Unknown option number.\n"
   :options
   [{:name "Display Customer Table"   :fn (partial print-table db :customers)}
    {:name "Display Product Table"    :fn (partial print-table db :products)}
    {:name "Display Sales Table"      :fn (partial print-table db :sales)}
    {:name "Total Sales for Customer" :fn #(print-sales-for-customer db (prompt "Enter customer name: "))}
    {:name "Total Count for Product"  :fn #(print-sales-for-product  db (prompt "Enter product name: "))}]})

(defn print-menu
  "Печатает на экран меню."
  [menu]
  (print (str/join "\n"
                   [(:title menu)
                    (dash-line (count (:title menu)))
                    (str/join "\n" (map-indexed
                                     (fn [i {name :name}]
                                       (format "%d. %s" (inc i) name))
                                     (:options menu)))
                    (format "%d. %s" (inc (count (:options menu))) (:exit menu))
                    ""
                    (:message menu)]))
  (flush))

(defn read-number
  "Читает число из стандартного ввода и парсит его, возвращает `nil` в случае
  ошибки парсинга."
  []
  (try (Integer/parseInt (read-line))
       (catch NumberFormatException _)))

(defn main-loop
  "Показывает меню и выполняет действия в зависимости от выбора пользователя."
  [menu]
  (loop []
    (print-menu menu)
    (let [option (read-number)]
      (cond
        ;; Не удалось распарсить номер опции. Скорее всего, введено не число.
        ;; Показываем текст ошибки.
        (nil? option)
        (do (println (:error menu))
            (recur))

        ;; Выбран один из пунктов меню. Вызваем функцию, которая определена в
        ;; меню.
        (<= 1 option (count (:options menu)))
        (do
          ((-> menu :options (nth (dec option)) :fn))
          (println)
          (recur))
        
        ;; Выбрана опция выхода. Ничего не делаем, выходим.
        (= option (inc (count (:options menu))))
        nil
        
        ;; В остальных ситуациях выводим сообщение об ошибке.
        :else
        (do (println (:error menu))
            (recur))))))

(defn -main
  []
  (main-loop (get-menu db)))
