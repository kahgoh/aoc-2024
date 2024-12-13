(ns part2)

(require '[clojure.java.io :as io])

(defn read-input [input-file]
  (with-open [reader (io/reader input-file)]
    (vec (map vec (line-seq reader)))))

(defn read-row-symbols [row-idx row-data]
  (filter #(not= nil %)
          (map #(let [symbol (get row-data %)]
                  (if (not= \. symbol) {symbol [[row-idx %]]} nil)) (range (count row-data)))))

(defn locate-symbols [input-map]
  (let [expanded (filter #(not-empty %)
                         (map #(read-row-symbols % (get input-map %)) (range (count input-map))))]
    (reduce (fn [x1 x2] (merge-with into x1 x2)) {} (flatten expanded))))

(defn expolate [from initial d-row d-col row-count col-count]
  (loop [[c-row c-col] from
         acc initial]
    (let [n-row (- c-row d-row)
          n-col (- c-col d-col)]
      (if (and (<= 0 n-row) (<= 0 n-col) (< n-row row-count) (< n-col col-count))
        (recur [n-row n-col] (conj acc [n-row n-col]))
        acc))))

(defn find-antinodes-between [[row1 col1] [row2 col2] row-count col-count]
  (let [d-row (- row2 row1)
        d-col (- col2 col1)
        partial (expolate [row2 col2] #{} d-row d-col row-count col-count)]
    (expolate [row1 col1] partial (- d-row) (- d-col) row-count col-count)))

(defn find-potential-antinodes [antennas row-count col-count]
  (loop [[next-node & remaining] antennas
         acc #{}]
    (if (or (empty? remaining) (= next nil)) acc
        (let [locations (reduce into acc (map #(find-antinodes-between next-node % row-count col-count) remaining))]
          (recur remaining locations)))))

(defn find-antinodes [antennas row-count col-count]
  (let [candidates (find-potential-antinodes antennas row-count col-count)]
    (filter (fn [[r c]] (and (<= 0 r) (<= 0 c) (< r row-count) (< c col-count))) candidates)))

(defn get-antinode-locations [antennas row-count col-count]
  (let [freqs (keys antennas)
        unique-locations (reduce into #{} (map #(find-antinodes (get antennas %) row-count col-count) freqs))]
    (count unique-locations)))

(defn solve2 [input-file]
  (let [antennas-map (read-input input-file)
        antennas (locate-symbols antennas-map)
        row-count (count antennas-map)
        col-count (count (get antennas-map 0))]
    (get-antinode-locations antennas row-count col-count)))

(solve2 "sample")
(solve2 "input")
