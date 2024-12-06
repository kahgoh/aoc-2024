(ns puzzle)

(require '[clojure.java.io :as io])

(defrecord Guard [pos direction])

(defn read-map [file]
  (with-open [reader (io/reader file)]
    (vec (map vec (line-seq reader)))))

(defn find-start [map]
  (loop [row-num 0]
    (let [row-data (get map row-num)
          row-length (count row-data)
          next-row-num (inc row-num)
          [col-num] (filter #(= (get row-data %) \^) (range row-length))]
      (if (not= nil col-num)
        [row-num col-num]
        (if (< next-row-num row-length) (recur next-row-num) nil)))))

(defn turn [from]
  (case from
    [-1 0] [0 1]
    [0 1] [1 0]
    [1 0] [0 -1]
    [0 -1] [-1 0]))

(defn simulate [map initial-guard]
  (loop [{[pos-row pos-col] :pos [dir-row dir-col] :direction :as guard} initial-guard
         visited #{}]
    (let [next-row (+ pos-row dir-row)
          next-col (+ pos-col dir-col)
          next-pos [next-row next-col]
          next-char (get-in map next-pos)
          next-visited (conj visited [pos-row pos-col])]
      (case next-char
        (\. \^) (recur (assoc guard :pos next-pos) next-visited)
        \# (recur (assoc guard :direction (turn (:direction guard))) next-visited)
        nil next-visited))))

(defn solve1 [input-file]
  (let [map (read-map input-file)
        start (find-start map)
        guard (Guard. start [-1 0])]
    (-> (simulate map guard) count)))

(solve1 "input")

(defn infinite-loop? [map initial-guard]
  (loop [{[pos-row pos-col] :pos [dir-row dir-col :as direction] :direction :as guard} initial-guard
         visited #{}]
    (let [next-row (+ pos-row dir-row)
          next-col (+ pos-col dir-col)
          next-pos [next-row next-col]
          next-char (get-in map next-pos)
          next-visited (conj visited [pos-row pos-col direction])]
      (case next-char
        (\. \^) (if (contains? visited [pos-row pos-col direction])
                  true
                  (recur (assoc guard :pos next-pos) next-visited))
        \# (recur (assoc guard :direction (turn direction)) next-visited)
        false))))

(defn solve2 [input-file]
  (let [maze (read-map input-file)
        start (find-start maze)
        guard (Guard. start [-1 0])
        candidates (-> (simulate maze guard) (disj start))]
    (->
     (filter #(infinite-loop? (update-in maze % (fn [_] \#)) guard) candidates)
     count)))
(solve2 "sample")
(solve2 "input")