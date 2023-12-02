(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests tap %]]
            ))

(def text-digits ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])
(defn is-string-a-number [inp]
  (not= nil (parse-long inp)))

(defn text-to-number [input]
  (str (inc (.indexOf text-digits input))))

(defn get-position-info-at-index [index input text-digit]
  (if (str/index-of input text-digit index)
    {:text-digit text-digit :index (str/index-of input text-digit index) :digit-to-be-replaced (text-to-number text-digit) }
    nil))

(defn get-position-info [input text-digit]
  (let [first-index (str/index-of input text-digit)
        last-index   (str/last-index-of input text-digit)]
    (if (not= first-index last-index)
      [(get-position-info-at-index 0 input text-digit) (get-position-info-at-index (dec last-index)  input text-digit)]
      [(get-position-info-at-index 0 input text-digit)])))

(defn get-all-position-data [currentline]
  (->> text-digits
       (map #(get-position-info currentline %1))
       (flatten)
       (filter not-empty)
       (sort-by :index  #(compare %2 %1))))

(defn replace-input-with[input position-info]
  (let [index (get position-info :index)
        digit-to-be-replaced (get position-info :digit-to-be-replaced)]
    (str (.substring input 0 index) digit-to-be-replaced  (.substring input (inc index) ))))

(defn convert-text-digit-to-normal-digit [input]
  (reduce #(replace-input-with %1 %2)
             input (get-all-position-data input)))

(defn first-and-last [inp]
  (let [first-one (first inp)
        last-one (last inp)]
    [first-one last-one]))

(defn calibrate [input]
  (->> input
       (convert-text-digit-to-normal-digit)
       (map str)
       (filter is-string-a-number)
       (first-and-last)
       (reduce  str)
       (parse-long)))

(defn read-as-array-from-classpath [filename]
  (->> filename
       (io/resource)
       (slurp)
       (str/split-lines)))

(defn find-total-calibration-value-in [filename]
  (->> filename
       (read-as-array-from-classpath)
       (map calibrate)
       (filter #(some? %1))
       (reduce +)))

(tests


  (calibrate "1abc2") := 12
  (calibrate "pqr3stu8vwx") := 38



  )

(comment

  (find-total-calibration-value-in "original.txt")
  (find-total-calibration-value-in "sample.txt")

  (find-total-calibration-value-in "originalpart2.txt")
  (find-total-calibration-value-in "samplepart2.txt")

  (reduce + '(12 38 15 77))


  (= 12 (calibrate "1abc2"))
  (= 38 (calibrate "pqr3stu8vwx"))
  (= 15 (calibrate "a1b2c3d4e5f"))
  (= 77 (calibrate "treb7uchet"))



  ;two1nine => 2wo19ine
  ;eightwothree
  ;abcone2threexyz
  ;xtwone3four
  ;4nineeightseven2
  ;zoneight234
  ;7pqrstsixteen

;2wo19ine
  ;eightwothree
  ;abcone2threexyz
  ;x2w1ne34our
  ;4nineeightseven2
  ;zoneight234
  ;7pqrstsixteen
  ;In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

  (get-position-info "two1nine" "two")
  (get-position-info "two1nine" "nine")
  (get-position-info "two1nine" "one")

  ; for a given input iterate through all text-digits and get me array of information

  ;text-digits

(.substring "two1nine" 1)




  (convert-text-digit-to-normal-digit "two1nine"   )

  ;2wo19ine


(replace-input-with "two1nine" {:text-digit "two", :index 0, :digit-to-be-replaced "2"} )
  (replace-input-with "2wo1nine" {:text-digit "nine", :index 4, :digit-to-be-replaced "9"} )





  ; A method should accept a digit
  ;   if digit is available  within input then
  ;       it should return {:digit "one" :index  :digit-to-be-replaced "1" } ;digits along with indexes
  ;   else
  ;     return nil

(text-to-number "one")


  (= 51 (calibrate "fiveightwone"))
  (= 11 (calibrate "one"))
  (= 12 (calibrate "onetwo"))
  (= 21 (calibrate "twone"))
  (= 29 (calibrate "two1nine"))
  (= 83 (calibrate "eightwothree"))
  (= 13 (calibrate "abcone2threexyz"))
  (= 24 (calibrate "xtwone3four"))
  (= 42 (calibrate "4nineeightseven2"))
  (= 14 (calibrate "zoneight234"))
  (= 76 (calibrate "7pqrstsixteen"))


  )


