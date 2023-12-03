(ns day3 (:require [clojure.java.io :as io]
                  [clojure.string :as str]
                  [hyperfiddle.rcf :refer [tests tap %]]))


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
  (let [start-index (str/index-of input (str digit) last-known-index)
        end-index (+ start-index  (.length  (str digit)))
        cols   (range start-index  end-index)
        positions (positions-for-row row cols)]
    {:value digit :positions positions :last-known-index end-index}))

(defn matrix-positions-for-all-digits-in-line [row input]
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

(defn is-valid-part-number? [digit-with-positions positions-for-symbols]
  (let [positions (digit-with-positions :positions)
        result (first (filter (fn [digitpos]
                         (find-in-symbols digitpos positions-for-symbols)
                         )  positions))
        valid-part-number? (some? result)]
    valid-part-number?))


(defn convert-to-key [item]
  (str "row" (item :row) "col" (item :col)))




(defn  convert-valid-part-numbers [symbols digits]
  (let [positions-for-symbols (flatten (map :positions symbols))]

  (reduce (fn [acc curr]
            (conj acc {:value (curr :value) :part-number (is-valid-part-number? curr positions-for-symbols)})
            ) [] digits )))


(defn process-engine-schema [filename]
  (let [symbols   (get-all-symbols-from-file filename )
        digits   (get-all-digits-from-file filename )
        results (convert-valid-part-numbers symbols digits)]
    (->> results
         (filter #(true? (%1 :part-number)))
         (map :value)
         (reduce +))))

(comment

  ;"Elapsed time: 10592.106386 msecs" default
  (process-engine-schema "day3part1sample.txt")
  (time  (process-engine-schema "day3part1full.txt"))
  (time  (process-engine-schema "day3part1full.txt"))






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
  (process-engine-schema "day3part1sample.txt") := 4361
  (process-engine-schema "day3part1full.txt") := 529618
  )