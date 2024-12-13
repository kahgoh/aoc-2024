(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.set :as set])

(defn read-garden-map [input-file]
  (with-open [content (io/reader input-file)]
    (vec (map vec (line-seq content)))))

(defn get-row-col-points [row-num row-content]
  (map-indexed (fn [index _] [row-num index]) row-content))

(defn get-points [garden-map]
  (let [garden-points (map-indexed get-row-col-points garden-map)]
    (reduce into #{} garden-points)))

(defn adjacent-points [[start-row start-col]]
  [[start-row (dec start-col)] [start-row (inc start-col)] [(dec start-row) start-col] [(inc start-row) start-col]])

(defn map-region-from [garden-map start]
  (let [req-type (get-in garden-map start)]
    (loop [[next-visit & to-visit] (adjacent-points start)
           acc #{start}]
      (let [next-elem (get-in garden-map next-visit)]
        (cond
          (nil? next-visit) acc
          (or (nil? next-elem) (not= next-elem req-type) (contains? acc next-visit)) (recur to-visit acc)
          :else (recur (into to-visit (adjacent-points next-visit)) (conj acc next-visit)))))))

(defn find-regions [garden-map]
  (loop [[start-point & remaining-points] (get-points garden-map)
         acc []]
    (if (nil? start-point)
      acc
      (let [region-points (map-region-from garden-map start-point)
            remaining-set (into #{} remaining-points)]
        (recur (set/difference remaining-set region-points) (into acc [region-points]))))))

(defn point-perimeter [region point]
  (count (filter #(not (contains? region %)) (adjacent-points point))))

(defn region-perimeter [region]
  (let [perimeters (map #(point-perimeter region %) region)]
    (reduce + perimeters)))

(defn get-price [region]
  (* (region-perimeter region) (count region)))

(defn solve1 [input-file]
  (let [garden-map (read-garden-map input-file)
        regions (find-regions garden-map)
        prices (map get-price regions)]
    (reduce + prices)))

; (solve1 "input")

(defn count-segments [points]
  (loop [req-value (inc (first points))
         [next & remaining] (rest points)
         count 1]
    (cond
      (nil? next) count
      (= req-value next) (recur (inc next) remaining count)
      :else (recur (inc next) remaining (inc count)))))

(defn count-faces [req-false req-true]
  (let [length (count req-false)
        contingent (filter #(and (not (get req-false %)) (get req-true %)) (range length))]
    (if (empty? contingent) 0 (count-segments contingent))))

(defn get-row-fill [region row-num col-start col-end]
  (vec (map #(contains? region [row-num %]) (range col-start (inc col-end)))))

(defn get-col-fill [region col-num row-start row-end]
  (vec (map #(contains? region [% col-num]) (range row-start (inc row-end)))))

(defn check-row-faces [region row-num start-col end-col]
  (let [before-row (get-row-fill region (dec row-num) start-col end-col)
        at-row (get-row-fill region row-num start-col end-col)
        after-row (get-row-fill region (inc row-num) start-col end-col)]
    (+ (count-faces before-row at-row) (count-faces after-row at-row))))
  
(defn check-col-faces [region col-num start-row end-row]
  (let [before-col (get-col-fill region (dec col-num) start-row end-row)
        at-col (get-col-fill region col-num start-row end-row)
        after-col (get-col-fill region (inc col-num) start-row end-row)]
    (+ (count-faces before-col at-col) (count-faces after-col at-col))))

(defn scan-rows [region start-row end-row start-col end-col]
  (reduce + (map #(check-row-faces region % start-col end-col) (range start-row (inc end-row)))))

(defn scan-cols [region start-row end-row start-col end-col]
  (reduce + (map #(check-col-faces region % start-row end-row) (range start-col (inc end-col)))))

(defn get-extremes [[[first-row first-col] & remaining-points]]
  (vec (reduce (fn [[min-row max-row min-col max-col] [next-row next-col]] [(min min-row next-row) (max max-row next-row) (min min-col next-col) (max max-col next-col)]) [first-row first-row first-col first-col] remaining-points)))

(defn count-faces-in-region [region]
  (let [[row-start row-end col-start col-end] (get-extremes region)]
    (+
      (scan-rows region row-start row-end col-start col-end)
      (scan-cols region row-start row-end col-start col-end))))

(defn get-price-2 [region]
  (* (count-faces-in-region region) (count region)))

(defn solve2 [input-file]
  (let [garden-map (read-garden-map input-file)
        regions (find-regions garden-map)
        prices (map get-price-2 regions)]
    (reduce + prices)))

(solve2 "input")
