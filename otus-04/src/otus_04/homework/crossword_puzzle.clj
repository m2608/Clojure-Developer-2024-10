(ns otus-04.homework.crossword-puzzle
  (:require [clojure.string :as str]))

;; Оригинал:
;; https://www.hackerrank.com/challengestr/crossword-puzzle/problem

;; Символ, который будет использоваться для отметки плейсхолдеров в кроссворде.
(def placeholder \-)

(defn get-placeholders-in-line
  "Возвращает список плейсхолдеров в строке в виде списков последовательных
  смещений для каждого символа.

  Пример:
  
  (get-placeholders-in-line “++----+-----”) ; [[2 3 4 5] [7 8 9 10 11]]
  "
  [line]
  (->> line
       ;; Разбиваем символы строки на группы.
       (partition-by (partial = placeholder))
       ;; Получаем пары символ - количество символов подряд.
       (map (juxt first count))
       ;; Получаем список плейсхолдеров в виде пар смещение - длина.
       (reduce (fn [[i places] [c n]]
                 [(+ i n)
                  (if (= c placeholder)
                    (conj places [i n])
                    places)])
               [0 []])
       second
       ;; Убираем плейсхолдеры из одного символа (это столбцы кроссворда).
       (remove (comp (partial = 1) second))
       ;; Преобразуем пары из [<смещение начала>, <длина>] в последовательность смещений
       ;; для каждого плейсхолдера.
       (mapv (comp vec
                   (partial apply range)
                   (juxt first (partial apply +))))))

(defn get-horizontal-placeholders 
  "Возвращает список плейсхолдеров в строках в виде пар списка кординат [<строка>, <столбец>]
  для строк."
  [lines]
  (->> lines
       ;; Для каждой строки получаем наборы плейсхолдеров.
       (map get-placeholders-in-line)
       ;; Добавляем к наборам номера строк.
       (map-indexed vector)
       ;; Убираем пустые строки.
       (remove (comp empty? second))
       ;; Получаем списки координат для каждой буквы.
       (map (fn [[row cols-list]]
              (for [cols cols-list
                    col cols]
                [row col])))))

(defn get-vertical-placeholders
  "Возвращает список плейсхолдеров в строках в виде пар списка кординат [<строка>, <столбец>]
  для столбцов."
  [lines]
  (->> (apply mapv str lines)
       get-horizontal-placeholders
       (map (fn [coords]
              (map (fn [[r c]]
                     [c r]) coords)))))

(defn parse-crossword
  "Парсит текстовое представление кроссворда. Определяет позиции плейсхолдеров.
  Возвращает:
 
  - список плейсхолдеров плейхолдеров (в виде набора координат)
  - вектор слов в порядке, соответствующем порядку плейсхолдеров 
  - размер кроссвора
  
  Слова группируются по количеству букв. Например:

  ```
  [(([0 1] [0 2] [0 3])
    ([5 0] [6 0] [7 0])
    ([2 0] [2 1] [2 2] [2 3]))
   [[\"aaa\" \"bbb\"] [\"xxxx\"]]
   [10 10]]
  ```"
  [data]
  (let [lines (str/split-lines data)]
    [(->> (butlast lines)
          ((juxt get-horizontal-placeholders get-vertical-placeholders))
          (apply concat)
          ;; Плейсхолдеры сортируем по возрастанию длины, чтобы их
          ;; порядок соответствовал порядку слов.
          (sort-by count))
     (->> (str/split (last lines) #";")
          ;; Сортируем слова по возрастанию длины, чтобы их порядок
          ;; соответствовал порядку плейсхолдеров.
          (sort-by count)
          ;; Разбиваем слова на группы по длине. При решении кроссворда
          ;; будем пробовать различные перестановки в этих группах и различные
          ;; комбинации групп.
          (partition-by count)
          (mapv (partial mapv str)))
     (->> (butlast lines)
          ((juxt count (comp count first))))]))

(defn factorial
  "Возвращает факториал."
  [n]
  (assert (>= n 0))
  (reduce * (range 1 (inc n))))

(defn remove-at
  "Удаляет элемент из вектора, находящийся на указанной позиции."
  [v pos]
  (assert (and (< pos (count v))
               (>= pos 0)))
  (into (subvec v 0 pos) (subvec v (inc pos))))

(defn permutations
  "Возвращает все возможные перестановки для набора значений `values`."
  [values]
  (mapv (fn [n]
          (->> (range (count values) 0 -1)
               (reduce (fn [[values permutations n] d]
                         (let [[new-n pos] ((juxt quot mod) n d)]
                           [(remove-at values pos)
                            (conj permutations (values pos))
                            new-n]))
                       [values [] n])
               second))
        (range (factorial (count values)))))

(defn combinations
  "Возвращает возможные комбинации значений из переданных наборов элементов.
  Каждый элемент считается вектором, при получении комбинации элементы конкатенируются.
  Например, для двух наборов `[[:a :b] [:c :d]] [[:s :t] [:x]]` функция вернет
  вектор возможных сочетаний:
  ```
  [[:a :b :s :t] [:a :b :x] [:c :d :s :t] [:c :d :x]]
  ```"
  [& args]
  (reduce
    (fn [result w]
      (->> result
           (mapv (fn [v]
                   (mapv (partial into v) w)))
           (reduce into)))
    args))

(defn check-consistency
  "Проверяет непротиворечивость соответствия букв и координат для
  нескольких групп."
  [letters-groups]
  (->>  letters-groups
       (reduce into [])
       (group-by first)
       ;; Нас интересуют только координаты, присутствующие в нескольких группах.
       (filter (comp (partial < 1) count second))
       ;; Во всех группах на одинаковых местах д.б. одинаковые буквы.
       (every? (fn [[_ letters]]
                 (apply = (mapv second letters))))))

(defn check-crossword
  "Проверяет решение кроссворда и в случае удачи возвращает список позиций букв.
  При неудаче возвращает `nil`."
  [placeholders words]
  (let [letters-groups (mapv zipmap placeholders words)]
    (when (check-consistency letters-groups)
      (apply merge letters-groups))))

(defn show-crossword
  "Возвращает текстовое представление решенного кроссворда."
  [[rows cols] letters-positions]
  (->> (range rows)
       (map (fn [r]
              (->> (range cols)
                   (map (fn [c]
                          (get letters-positions [r c] "+")))
                   (apply str))))
       (str/join "\n")))

(defn solve
  "Возвращает решённый кроссворд. Аргумент является строкой вида

  +-++++++++
  +-++++++++
  +-++++++++
  +-----++++
  +-+++-++++
  +-+++-++++
  +++++-++++
  ++------++
  +++++-++++
  +++++-++++
  LONDON;DELHI;ICELAND;ANKARA

  Все строки вплоть до предпоследней описывают лист бумаги, а символами
  '-' отмечены клетки для вписывания букв. В последней строке перечислены
  слова, которые нужно 'вписать' в 'клетки'. Слова могут быть вписаны
  сверху-вниз или слева-направо."
  [input]
  (let [[placeholders words size] (parse-crossword input)
        words-combinations (apply combinations (map permutations words))]
    (show-crossword size (some (partial check-crossword placeholders)
                               words-combinations))))

