(ns day2
(:require [clojure.java.io :as io]
  [clojure.string :as str]
  [hyperfiddle.rcf :refer [tests tap %]]
  ))

(def max-cubes-in-bag {:red 12 :green 13 :blue 14} )
(defn game-id [input]
  (-> input
      (str/split  #":" )
      (first)
      (str/replace  #"[^0-9]" "")
      (parse-long)))

(defn color-for [input]
    (cond
      (str/includes? input "blue") :blue
      (str/includes? input "red") :red
      (str/includes? input "green") :green ))

(defn cubes-used [input]
  (parse-long (str/replace input #"[^0-9]" "")))

(defn fetch-cube-data [input]
  (let [color (color-for input)
        count (cubes-used input)]
    {color count}))

(defn game-round-info [input]
  (let [revelations (str/split input #"," )]
    (reduce  #(conj %1 (fetch-cube-data %2)) {} revelations) ))

(defn game-info [input]
  (let [rounds (str/split   (nth  (str/split input #":" ) 1) #";" )]
    (map game-round-info rounds)))

(defn is-acceptable-value? [curr key]
  (if (contains? curr key)
    (<= (curr key) (max-cubes-in-bag key))
    true))

(defn is-acceptable-game-info [game-data]
  (reduce (fn [acc,curr]
            (and acc  (is-acceptable-value? curr :red )
                 (is-acceptable-value? curr :green )
                 (is-acceptable-value? curr :blue )))
            true game-data))

(defn analyse-game [input]
  (let [gi (game-info input)
        id (game-id input)]
    {:id id :valid (is-acceptable-game-info gi)}))

(defn read-as-array-from-classpath [filename]
  (->> filename
       (io/resource)
       (slurp)
       (str/split-lines)))

(defn analyse-games-from-file [filename]
  (let [lines  (read-as-array-from-classpath filename)
        analysis-results (map analyse-game lines)
        filtered-results      (filter #(true? (%1 :valid)) analysis-results )        ]

    (reduce (fn [acc,curr]
              (+ acc (curr :id))
              ) 0 filtered-results )))

(defn is-greater-number-of-colors? [acc cur key]
  (or (and (contains? cur key)
           (false?  (contains? acc key))
           )  (and (contains? cur key)
                   (contains? acc key)
                   (< (acc key) (cur key)))))

(defn concat-if-valid [acc curr key]
  (if (is-greater-number-of-colors? acc curr key)
    (conj acc {key (get curr key)} )
    acc))

(defn find-least-colors-in-game [input]
  (reduce (fn [acc curr]
            (-> (concat-if-valid acc curr :red)
                (concat-if-valid curr :blue)
                (concat-if-valid curr :green)
                ))
          {}
          input))
(defn find-least-colors-in-line [input]
  (->> (game-info input)
       find-least-colors-in-game))

(defn product-of [input]
  (reduce-kv #(* %1 %3)
             1
             (find-least-colors-in-line input)))

(defn power-of-sets-from-file [filename]
  (let [lines  (read-as-array-from-classpath filename)
        products (map product-of lines)
         result (reduce + products)]
    result))

(tests
  (game-id "Game 11: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") := 11
  (game-id "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") := 1
  (color-for "3 blue" ) := :blue
  (color-for "4 red" ) := :red
  (cubes-used "4 red" ) := 4
  (cubes-used " 20 red" ) := 20
  (fetch-cube-data " 20 red" ) := {:red 20}
  (fetch-cube-data "2 green" ) := {:green 2}
  (game-round-info " 3 blue, 4 red") := {:blue 3, :red 4}
  (game-round-info "5 blue, 4 red, 13 green") := {:blue 5, :red 4, :green 13}
  (game-info "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") := '({:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2})
  (game-info "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red") := '({:green 1, :red 3, :blue 6} {:green 3, :red 6} {:green 3, :blue 15, :red 14})
  (is-acceptable-value? {:green 3, :red 26} :red) := false
  (is-acceptable-value? {:green 3, :red 6} :red) := true
  (is-acceptable-value? {:green 3 } :red) := true
  (is-acceptable-game-info '({:green 1, :red 3, :blue 6} {:green 3, :blue 14, :red 12} {:green 3, :red 6} )) := true
  (is-acceptable-game-info '({:green 1, :red 33, :blue 6} {:green 3, :blue 14, :red 12} {:green 3, :red 6} )) := false
  (analyse-game "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red") := {:id 4, :valid false}
  (analyse-game "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 12 blue, 12 red") := {:id 4, :valid true}
  (analyse-games-from-file "day2sample.txt") := 8
  (analyse-games-from-file "day2original.txt") := 2377
  (find-least-colors-in-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") := {:red 4, :blue 6, :green 2}
  (find-least-colors-in-line "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue") := {:blue 4, :green 3, :red 1}
  (find-least-colors-in-line "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red") := {:red 20, :blue 6, :green 13}
  (find-least-colors-in-line "Game 3: 8 green;  4 red, 13 green; 5 green, 1 red") := {:green 13, :red 4}
  (product-of "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green") := 48
  (product-of "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue") := 12
  (product-of "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red") := 1560
  (product-of "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red") := 630
  (power-of-sets-from-file "day2samplepart2.txt") := 2286
  (power-of-sets-from-file "day2originalpart2.txt") := 71220

  )