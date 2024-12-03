(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])

(defn read-line [content]
  (edn/read-string (str "[" content "]")))

(defn unzip [items]
  (reduce #(let [[coll1 coll2] %1
                 [elem1 elem2] %2] 
             [(conj coll1 elem1) (conj coll2 elem2)]) [[][]] items))

(defn read-content [line-seq]
  (unzip (map read-line line-seq)))

(defn diff [[item1 item2]] (abs (- item2 item1)))

(defn solve1 [[coll1 coll2]]
  (let [sorted1 (sort coll1)
        sorted2 (sort coll2)
        zipped (map vector sorted1 sorted2)]
        (reduce #(+ %1 (diff %2)) 0 zipped)))

(defn score-matches [value coll]
  (reduce #(+ %1 (if (= value %2) value 0)) 0 coll))

(defn solve2 [[coll1 coll2]]
  (reduce #(+ %1 (score-matches %2 coll2)) 0 coll1))

(defn load-input [file]
  (with-open [content (io/reader file)]
    (read-content (line-seq content))))

(defn puzzle1 [input]
  (solve1 (load-input input)))

(defn puzzle2 [input] (solve2 (load-input input)))

(puzzle1 "puzzle-input")
(puzzle2 "puzzle-input")