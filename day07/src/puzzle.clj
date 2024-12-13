(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[clojure.edn :as edn])
(require '[clojure.math :as math])

(defn parse-line [line]
  (let [[total-text values-text] (str/split line #":" )
        values (edn/read-string(str "[" values-text "]"))]
    [(Long/parseLong total-text) values]))

(defn get-next-totals [req-total cur-totals next-value]
  (let [next-totals (flatten (map (fn [val] [(+ val next-value) (* val next-value)]) cur-totals))]
    (vec (filter #(<= % req-total) next-totals))))

(defn test-makeable [req-total [first & values]]
  (loop [totals [first]
         [next-value & rem-values] values]
    (if (= next-value nil) 
      false
      (let [next-totals (get-next-totals req-total totals next-value)]
        (if (.contains next-totals req-total)
          true
          (recur next-totals rem-values))))))

(defn solve1 [input-file]
  (with-open [input-content (io/reader input-file)]
    (let [equations (vec (map parse-line (line-seq input-content)))
          filtered (filter (fn [[total values]] (test-makeable total values)) equations)] 
      (reduce + (map first filtered)))))

(solve1 "input")

(defn digits [num]
  (loop [remaining num
         count 0]
    (if
     (> remaining 0)
      (recur (quot remaining 10) (inc count)) count)))

(defn append [num1 num2]
  (let [places (digits num2)]
    (+ (* num1 (long (math/pow 10 places))) num2)))

(defn get-next-totals2 [req-total cur-totals next-value]
  (let [next-totals (flatten (map (fn [val] [(+ val next-value) (* val next-value) (append val next-value)]) cur-totals))]
    (vec (filter #(<= % req-total) next-totals))))

(defn test-makeable2 [req-total [first & values]]
  (loop [totals [first]
         [next-value & rem-values] values]
    (if (= next-value nil)
      false
      (let [next-totals (get-next-totals2 req-total totals next-value)]
        (if (.contains next-totals req-total)
          true
          (recur next-totals rem-values))))))

(defn solve2 [input-file]
  (with-open [input-content (io/reader input-file)]
    (let [equations (vec (map parse-line (line-seq input-content)))
          filtered (filter (fn [[total values]] (test-makeable2 total values)) equations)]
      (reduce + (map first filtered)))))

(solve2 "input")