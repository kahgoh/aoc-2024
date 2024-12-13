(ns part2)

(defn input-to-seq [input]
  (map #(Character/digit % 10) input))

(defn expand [idx entry]
  [[idx (first entry)] [nil (second entry)]])

(defn get-model [input]
  (let [sequence (partition 2 2 [0] (input-to-seq input))]
    (reduce into [] (map-indexed expand sequence))))

(defn join-gaps [model]
  (loop [[[next1-id next1-size :as next1] & remaining1] model
         acc []]
    (let [[[next2-id next2-size :as next2] & remaining2] remaining1]
      (cond
        (= nil next1) acc
        (= nil next2) (conj acc next1)
        (= next1-id next2-id) (recur (conj remaining2 [next1-id (+ next1-size next2-size)]) acc)
        :else (recur remaining1 (conj acc next1))))))

(defn get-next-remaining [init-remaining rem-size [_ target-size :as target-item]]
  (let [scrubbed (join-gaps (map #(if (= % target-item) [nil target-size] %) init-remaining))]
    (if (> rem-size 0)
      (into [[nil rem-size]] scrubbed)
      scrubbed)))

(defn rearrange [model target-item]
  (let [[_ target-size] target-item]
    (loop [[[next-id next-size :as next-item] & remaining] model
           acc []]
      (cond
        ; Can't have matched, so answer is the model again
        (= target-item next-item) model

        ; Found a gap big enough
        (and (nil? next-id) (<= target-size next-size))
        (vec (concat acc [target-item] (get-next-remaining remaining (- next-size target-size) target-item)))

        ; Keep looking
        :else (recur remaining (conj acc next-item))))))

(defn defrag [model]
  (loop [[[item-id _ :as queue-item] & queue-remaining] (reverse model)
         acc model]
    (cond
      (nil? queue-item) acc
      (nil? item-id) (recur queue-remaining acc) 
      :else (recur queue-remaining (rearrange acc queue-item)))))

(defn expand-model [compressed-model]
  (flatten (map #(repeat (second %) (first %)) compressed-model)))

(defn checksum [model]
  (reduce + (map-indexed (fn [idx item] (if (nil? item) 0 (* idx item))) model)))

(defn solve2 [input]
   (-> (get-model input) (vec) (defrag) (expand-model) (checksum)))

; (solve2 "2333133121414131402")
(solve2 (slurp "input"))