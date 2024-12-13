(ns puzzle)

(require '[clojure.edn :as edn])
(require '[clojure.string :as string])

(defn string-to-vec [input]
  (edn/read-string (str "[" input "]")))

(defn blink-stone
  ([stone]
   (let [stone-str (str stone)
         stone-digits (count stone-str)
         half-digits (quot stone-digits 2)]
     (cond
       (= 0 stone) [1]
       (= 0 (rem stone-digits 2)) [(Long/parseLong (.substring stone-str 0 half-digits)) (Long/parseLong (.substring stone-str half-digits))]
       :else [(* stone 2024)]))))

(defn blink-acc [init-acc [stone init-count]]
  (reduce (fn [acc next-stone] (merge-with + acc {next-stone init-count})) init-acc (blink-stone stone)))

(defn blink-seq [stone-counts]
  (reduce blink-acc {} stone-counts))

(defn run-blinks [stone-string times]
  (let [initial (string-to-vec stone-string)
        map-counts (reduce (fn [counts elem] (merge-with + counts {elem 1})) {} initial)]
    (reduce (fn [counts _] (blink-seq counts)) map-counts (range times))))

(defn count-stones [stone-string times]
  (reduce (fn [sum [_ count]] (+ sum count)) 0 (run-blinks stone-string times)))

;; Part 1
;; (count-stones (slurp "input") 25)

;; Part 2
(count-stones (slurp "input") 75)