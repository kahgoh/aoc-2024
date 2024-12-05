(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])

(defn read-line [content]
  (edn/read-string (str "[" content "]")))

(defn test-values [content]
  (loop [[elem1 & remaining] content]
      (let
       [[elem2] remaining
        diff (- elem2 elem1)
        test-result (and (< 0 diff) (< diff 4))]
        (if (and test-result (> (count remaining) 1))
          (recur remaining)
          test-result))))

(defn check-line [content]
  (let [numbers (read-line content)]
    (or (test-values numbers) (test-values (reverse numbers)))))

(defn solve1 [file]
  (with-open [content (io/reader file)]
    (count (filter check-line (line-seq content)))))

(defn gen-candidates [source-coll]
  (loop [head []
         [current & tail] source-coll
         acc []]
    (let [next-candidate (concat head tail)
          next-acc (conj acc next-candidate)]
         (if (seq tail) (recur (conj head current) tail next-acc) next-acc))))

(defn dampen-check [content]
  (let [numbers (read-line content)]
    (loop [[next & tail] (gen-candidates numbers)]
      (if (or (test-values next) (test-values (reverse next)))
        true
        (if (seq tail) (recur tail) false)))))

(defn solve2 [file]
  (with-open [content (io/reader file)]
    (count (filter dampen-check (line-seq content)))))

(solve1 "sample")
(solve1 "input")