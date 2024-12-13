(ns puzzle)

(defn input-to-seq [input]
  (map #(Character/digit % 10) input))

(defn expand-block [id raw-data]
  (let [fill-length (first raw-data)
        empty-length (second raw-data)] 
    (concat (repeat fill-length id) (repeat empty-length nil))))

(defn get-initial-model [input]
  (let [sequence (partition 2 2 [0] (input-to-seq input))]      
    (flatten (map-indexed expand-block sequence))))

(defn defrag [initial]
  (loop [idx 0
         [queue-next & queue-remaining :as queue] initial
         acc []
         [stack-next & stack-remaining :as stack] (reverse initial)]
         (let [stack-idx (dec (count stack))]
         (cond
           (> idx stack-idx) acc
           (= nil stack-next) (recur idx queue acc stack-remaining)
           (= nil  queue-next) (recur (inc idx) queue-remaining (conj acc stack-next) stack-remaining)
           :else (recur (inc idx) queue-remaining (conj acc queue-next) stack)))))

(defn checksum [model]
  (reduce + (map-indexed * model)))

(defn solve1 [input]
  (-> (get-initial-model input) (defrag) (checksum)))

(solve1 "2333133121414131402")
(solve1 (slurp "input"))