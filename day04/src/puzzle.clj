(ns puzzle)

(require '[clojure.java.io :as io])

(defn read-content [file]
  (with-open [reader (io/reader file)]
    (vec (map vec (line-seq reader)))))

(defn get-point [matrix i j]
  (-> matrix (get i []) (get j \.)))

(defn collect [matrix i j di dj]
  (vec (map #(let [ti (+ i di (* di %))
                   tj (+ j dj (* dj %))]
               (get-point matrix ti tj)) (range 3))))

(defn count-matches [matrix i j]
  (let [at-point (get-point matrix i j)
        values [(collect matrix i j -1 -1)
                (collect matrix i j -1 0)
                (collect matrix i j -1 1)
                (collect matrix i j 0 -1)
                (collect matrix i j 0 1)
                (collect matrix i j 1 -1)
                (collect matrix i j 1 0)
                (collect matrix i j 1 1)]]
    (if (= at-point \X) (-> (filter #(= % [\M \A \S]) values) count) 0)))

(defn count-for-row [matrix row-num]
  (let [row-data (get matrix row-num)
        col-count (count row-data)]
    (map #(count-matches matrix row-num %) (range col-count))))

(defn solve1 [file]
  (let [data (read-content file)
        row-count (count data)]
    (reduce + (flatten (map #(count-for-row data %) (range row-count))))))

(solve1 "input")

(defn cross-filter [matrix i j]
  (let [d1 [(get-point matrix (dec i) (dec j)) (get-point matrix (inc i) (inc j))]
        d2 [(get-point matrix (inc i) (dec j)) (get-point matrix (dec i) (inc j))]
        s1 (sort d1)
        s2 (sort d2)]
    (if (= \A (get-point matrix i j))
      (= s1 s2 [\M \S])
      false)))

(defn count-crosses [matrix row-num]
  (let [row (get matrix row-num)
        col-count (count row)]
    (-> (filter #(cross-filter matrix row-num %) (range col-count)) count)))

(defn solve2 [file]
  (let [data (read-content file)
        row-count (count data)]
    (reduce #(+ %1 (count-crosses data %2)) (range row-count))))

(solve2 "sample")
(solve2 "input")