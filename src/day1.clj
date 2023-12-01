(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;Inputs
  ;1abc2
  ;pqr3stu8vwx
  ;a1b2c3d4e5f
  ;treb7uchet

;In this example,
  ; the calibration values of these four lines are 12, 38, 15, and 77.
  ; Adding these together produces 142.

(defn is-string-a-number [inp]
  (not= nil (parse-long inp)))


(defn first-and-last [inp]
  (let [first-one (first inp)
        last-one (last inp)]
    [first-one last-one]))

(defn calibrate [input]
  (->> input
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

(comment

  (find-total-calibration-value-in "original.txt")
  (find-total-calibration-value-in "sample.txt")

(reduce + '(12 38 15 77))
  ; check the first and last
  (first-and-last '("1" "2" "3" "4" "5"))


  (calibrate "a1b2c3d4e5f")
(calibrate "abc")
  (= 12 (calibrate "1abc2"))
  (= 38 (calibrate "pqr3stu8vwx"))
  (= 15 (calibrate "a1b2c3d4e5f"))

  (= 77 (calibrate "treb7uchet"))




  (println "hello World")


  )
