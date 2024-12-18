(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])

(defn read-placements [input-file count]
  (with-open [reader (io/reader input-file)]
    (into #{} (map #(edn/read-string (str "[" % "]")) (take count (line-seq reader))))))

(def directions [[-1 0] [0 1] [1 0] [0 -1]])

(defn add-adjacent-spaces [from-point queue placements rows cols step-count]
  (let [candidates (mapv (fn [dir] (mapv + from-point dir)) directions)
        in-bounds (filter (fn [[c r]] (and (<= 0 r) (<= 0 c) (< r rows) (< c cols))) candidates)
        unblocked (filter #(not (contains? placements %)) in-bounds)]
    (into queue (map (fn [point] [step-count point]) unblocked))))

(defn counts [input-file bytes rows cols]
  (let [placements (read-placements input-file bytes)] 
    (loop [[[step-num next :as elem] & queue] [[0 [0 0]]]
           step-counts {}]
      (cond
        (nil? elem) step-counts
        (= next [(dec rows) (dec cols)]) (assoc step-counts next step-num)
        (contains? step-counts next) (recur queue step-counts)
        :else (recur (add-adjacent-spaces next (vec queue) placements rows cols (inc step-num)) (assoc step-counts next step-num))))))

(def puzzle-counts (counts "input" 1024 71 71))
(println (get puzzle-counts [70 70]))

; Part 2 with the input. First find the line number, then get the last item at the line
; NOTE: Make sure read-placements reads into vector instead of a set!

(loop [line-num 1025] 
  (let [count-map (counts "input" line-num 71 71)]
    (if (nil? (get count-map [70 70])) line-num (recur (inc line-num)))))

(last (read-placements "input" 2915))

; Part 2 with the sample. First find the line number, then get the last item at the line
; NOTE: Make sure read-placements reads into vector instead of a set!

(loop [line-num 12]
  (let [count-map (counts "sample" line-num 7 7)]
    (if (nil? (get count-map [6 6])) line-num (recur (inc line-num)))))

(last (read-placements "sample" 21))