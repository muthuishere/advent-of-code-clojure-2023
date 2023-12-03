(ns day3 (:require [clojure.java.io :as io]
                  [clojure.string :as str]
                  [hyperfiddle.rcf :refer [tests tap %]]
                  ))


;The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
;
;Here is an example engine schematic:
;
;467..114..
;...*......
;..35..633.
;......#...
;617*......
;.....+.58.
;..592.....
;......755.
;...$.*....
;.664.598..
;In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.
;
;Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

;tasks
; Need to find digits within a line
; Need to find symbols within a line
; need to numbers adjacent to symbols across all lines
; need to filter non adjacent numbers
; sum it
(defn is-string-a-number [inp]
  (not= nil (parse-long inp)))

(defn non-digits-to-dots [input]
  (str/replace input #"[^0-9]" ".")
  )
(defn digits-to-dots [input]
  (str/replace input #"[0-9]" ".")
  )
(defn only-digits [input]
  (str/replace input #"[^0-9]" ""))

(defn only-non-digits [input]
  (str/replace input #"[0-9]" ""))

(defn is-not-empty-string? [input]
  ( false? (.isEmpty input) ))

(defn read-as-array-from-classpath [filename]
  (->> filename
       (io/resource)
       (slurp)
       (str/split-lines)))

(defn read-as-indexed-array-from-classpath [filename]
  (let [lines (read-as-array-from-classpath filename)
        ranges (range (count lines))]
          (map (fn [index]
             {:row (inc index)  :line (nth lines index)})
               ranges)))

(defn digits-within-line [input]
  (->> (str/split (non-digits-to-dots input )  #"\.")
       (map only-digits)
       (filter is-string-a-number)
       (map parse-long)))


(defn symbols-within-line [input]
  (->> (str/split (digits-to-dots input ) #"\.")
       (map only-non-digits)
       (filter  is-not-empty-string?)))

(defn positions-for-row [row cols]
  (reduce  #(conj %1 {:row row :col %2}) [] cols)
  )

(defn matrix-positions-for-digit [{:keys [row digit input last-known-index]}]
  ;(println "==========")
  ;(println row)
  ;(println digit)
  ;(println input)
  ;(println last-known-index)

  ;(println (str/index-of input (str digit) last-known-index))
  (let [start-index (str/index-of input (str digit) last-known-index)
        end-index (+ start-index  (.length  (str digit)))
        cols   (range start-index  end-index)
        positions (positions-for-row row cols)]
    {:value digit :positions positions :last-known-index end-index}))
(comment

  (let [
        input "...267*537.................850..864.262..........178..*..+......721.......314..............$........834.........%..........@................"
        last-known-index 0
        digit

        ])
  )
(defn matrix-positions-for-all-digits-in-line [row input]
  ; if there are same digits appearing twice , I need index to send as well ?

  (->> (digits-within-line input)
       (reduce (fn [acc curr]
                 (let [last-val (get  (last acc) :last-known-index 0)
                       last-known-index (if (nil? last-val) 0 last-val)]
                  (conj acc (matrix-positions-for-digit {:row row :digit curr :input input :last-known-index last-known-index} )))) [] )))



(defn matrix-positions-for-symbol [{:keys [row engine-symbol input last-known-index]}]
  (let [col (str/index-of input engine-symbol last-known-index)
        end-index (inc col)
        start-row (dec row)
        end-row (inc  (inc row))
        start-col (dec col)
        end-col (inc (inc col))
        rows (range start-row end-row)
        cols  (range start-col end-col)
        positions (reduce  #(conj %1 (positions-for-row %2 cols)) [] rows) ]
    {:value engine-symbol :positions positions :last-known-index end-index}))


(defn matrix-positions-for-all-symbols-in-line [row input]
  (->> (symbols-within-line input)
       (reduce (fn [acc curr]
                 (let [last-val (get  (last acc) :last-known-index 0)
                       last-known-index (if (nil? last-val) 0 last-val)]
                   (conj acc (matrix-positions-for-symbol {:row row :engine-symbol curr :input input :last-known-index last-known-index} )))) [] )))


(defn get-all-symbols-from-file [filename]
  (let [lines (read-as-indexed-array-from-classpath filename)
        all-symbols (reduce (fn [acc curr]
                              (let [res (matrix-positions-for-all-symbols-in-line (curr :row) (curr :line))]
                                (if (empty? res)
                                  acc
                                  (conj acc   res ))))  []  lines)
        ]
    (flatten all-symbols)
    ))
(defn get-all-digits-from-file [filename]
  (let [lines (read-as-indexed-array-from-classpath filename)
        all-digits (reduce (fn [acc curr]
                             (let [res (matrix-positions-for-all-digits-in-line (curr :row) (curr :line))]
                               (if (empty? res)
                                 acc
                                 (conj acc   res ))))  []  lines)]
    (flatten all-digits)))

(defn is-same-row-and-col [sympos digitpos]
  (let [symrow (sympos :row)
        symcol (sympos :col)
        digitrow (digitpos :row)
        digitcol (digitpos :col)]
    (and (= symrow digitrow)
         (= symcol digitcol))))
(defn find-in-symbols [digitpos positions-for-symbols]
  (->> positions-for-symbols
       (filter #(is-same-row-and-col %1 digitpos ) )
       first))

(defn is-valid-part-number? [digit-with-positions symbols]
  (let [positions (digit-with-positions :positions)
        positions-for-symbols (flatten (map :positions symbols))
        result (filter (fn [digitpos]
                         (find-in-symbols digitpos positions-for-symbols)
                         )  positions)
        valid-part-number? (not (empty? result))]
    valid-part-number?))

(defn convert-valid-part-numbers [symbols digits]
  (reduce (fn [acc curr]
            (conj acc {:value (curr :value) :part-number (is-valid-part-number? curr symbols)})
            ) [] digits ))
(defn process-engine-schema [filename]
  (let [symbols   (get-all-symbols-from-file filename )
        digits   (get-all-digits-from-file filename )
        results (convert-valid-part-numbers symbols digits)
        ]

    (->> results
         (filter #(true? (%1 :part-number)))
         (map :value)
         (reduce +))

    ))

(comment


  (process-engine-schema "day3part1sample.txt")
  (process-engine-schema "day3part1full.txt")


  (let [
        filename "day3part1sample.txt"
        symbols   (get-all-symbols-from-file filename )
        digits   (get-all-digits-from-file filename )]
    (reduce (fn [acc curr]
              (conj acc {:value (curr :value) :part-number (is-valid-part-number? curr symbols)})
              ) [] digits ))



  (let [
        filename "day3part1sample.txt"
        symbols   (get-all-symbols-from-file filename )
        digits   (get-all-digits-from-file filename )]
    {:symbols symbols :digits digits}

)


  ;[
  ; [{:value "*",
  ;   :positions [[{:row 1, :col 2} {:row 1, :col 3} {:row 1, :col 4}]
  ;               [{:row 2, :col 2} {:row 2, :col 3} {:row 2, :col 4}]
  ;               [{:row 3, :col 2} {:row 3, :col 3} {:row 3, :col 4}]],
  ;   :last-known-index 4}]
  ; [{:value "#",
  ;   :positions [[{:row 3, :col 5} {:row 3, :col 6} {:row 3, :col 7}]
  ;               [{:row 4, :col 5} {:row 4, :col 6} {:row 4, :col 7}]
  ;               [{:row 5, :col 5} {:row 5, :col 6} {:row 5, :col 7}]],
  ;   :last-known-index 7}]
  ;
  ;
  ; [{:value 467, :positions [{:row 1, :col 0} {:row 1, :col 1} {:row 1, :col 2}], :last-known-index 3}
  ;  {:value 114, :positions [{:row 1, :col 5} {:row 1, :col 6} {:row 1, :col 7}], :last-known-index 8}]
  ; [{:value 35, :positions [{:row 3, :col 2} {:row 3, :col 3}], :last-known-index 4}
  ;  {:value 633, :positions [{:row 3, :col 6} {:row 3, :col 7} {:row 3, :col 8}], :last-known-index 9}]
  ; [{:value 617, :positions [{:row 5, :col 0} {:row 5, :col 1} {:row 5, :col 2}], :last-known-index 3}]



   ;{:digit digit :is-part-number true}

  ; for each digit found , Find its a part number or not by looking through all the symbols


  ; get All Symbols assign
  ;get All Digits
  ; For each Digit, Find the position collides with any one of the symbol
  ; if its then true


  ;need to numbers adjacent to symbols across all lines
  ;;467..114..
  ;;...*......
  ;;..35..633.


  (str/index-of "...$.*..*." "*" )
  (str/index-of "...$.**." "*" 0 )
  (str/index-of "...$.**." "*" 6 )

  (matrix-positions-for-all-symbols-in-line 0 "...$.*..*.")


  (matrix-positions-for-digit 0 467 "467..114..") := {:value 467, :positions [{:row 0, :col 0} {:row 0, :col 1} {:row 0, :col 2}]}
  (matrix-positions-for-digit 0 114 "467..114..") := {:value 467, :positions [{:row 0, :col 0} {:row 0, :col 1} {:row 0, :col 2}]}

  ; {:value 467 :positions }

  ;matrix-positions-for-all-digits-in-line , will get a row and a line


  (let [
        all-digits (digits-within-line "114..114..")


        ]
    ; get Digits with indexes

    )





  )

(tests
  (digits-within-line "467..114..") := '(467 114)
  (digits-within-line "617*......") := '(617)
  (digits-within-line "......755.") := '(755)
  (digits-within-line "...*......") := '()
  (digits-within-line "...$.*....") := '()
  (symbols-within-line "...$.*....") := '("$" "*")
  (symbols-within-line "467..114..") := '()
  (symbols-within-line "...*......") := '( "*")
  (matrix-positions-for-all-digits-in-line 0 "114..114..") := [{:value 114, :positions [{:row 0, :col 0} {:row 0, :col 1} {:row 0, :col 2}], :last-known-index 3}
                                                               {:value 114, :positions [{:row 0, :col 5} {:row 0, :col 6} {:row 0, :col 7}], :last-known-index 8}]

  (matrix-positions-for-all-digits-in-line 0 "467..114..") := [{:value 467, :positions [{:row 0, :col 0} {:row 0, :col 1} {:row 0, :col 2}], :last-known-index 3}
                                                               {:value 114, :positions [{:row 0, :col 5} {:row 0, :col 6} {:row 0, :col 7}], :last-known-index 8}]
  (matrix-positions-for-all-digits-in-line 0 "...$.*....") := []
  )