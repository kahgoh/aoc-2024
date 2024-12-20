(ns part2)
(require '[clojure.java.io :as io])

(defrecord Reindeer [from-point current-point head score])
(defrecord Crumb [from score])

(def directions [:north :east :south :west])

(defn find-index [coll item]
  (loop [idx 0]
    (let [elem (get coll idx)]
      (cond
        (nil? elem) nil
        (= elem item) idx
        :else (recur (inc idx))))))

(defn turn-cost [from-head to-head]
  (let [from-head-idx (find-index directions from-head)
        to-head-idx (find-index directions to-head)
        diff (abs (- from-head-idx to-head-idx))]
    (if
       ;; Turn from west to north or vice versa
     (= (sort [from-head-idx to-head-idx]) [0 3])
      1000
      (* diff 1000))))

(defn read-maze [input-file]
  (let [content (io/reader input-file)]
    (mapv vec (line-seq content))))

(defn find-in-row [row-num row-data target]
  (let [col-num (find-index row-data target)]
    (if (nil? col-num) nil [row-num col-num])))

(defn find-point [maze-map target]
  (loop [row-num 0]
    (let [row-content (get maze-map row-num)]
      (if (nil? row-content)
        nil
        (let [try-row (find-in-row row-num row-content target)]
          (if (nil? try-row) (recur (inc row-num)) try-row))))))

(def dir-offsets [[:north [-1 0]] [:east [0 1]] [:south [1 0]] [:west [0 -1]]])

(defn add-dir-offset [from-point [dir offset]]
  [dir (mapv + from-point offset)])

(defn get-adjacent-spaces [maze-map from-point at-point]
  (let [adjacent-spaces (mapv (fn [elem] (add-dir-offset at-point elem)) dir-offsets)]
    (filter (fn [[_ test-point]] (and (not= test-point from-point) (not= \# (get-in maze-map test-point)))) adjacent-spaces)))

(defn score-move [from-head to-head]
  (+ (turn-cost from-head to-head) 1))

(defn add-move [{current-head :head current-point :current-point current-score :score} [to-head to-point]]
  (let [new-score (+ current-score (score-move current-head to-head))]
    (Reindeer. current-point to-point to-head new-score)))

(defn add-possibilities [maze-map {from :from-point current :current-point :as from-reindeer} acc]
  (let [possible-moves (get-adjacent-spaces maze-map from current)
        next-moves (map #(add-move from-reindeer %) possible-moves)]
    (into (vec acc) next-moves)))

(defn move-reindeer [maze-map start-point start-head end-point]
  (loop [[next-point & other-points] [(Reindeer. start-point start-point start-head 0)]
         seen {}]
    (let [{from-point :from-point current-point :current-point score :score} next-point
          exist-crumb (get seen current-point)
          {end-score :score} (get seen end-point)]
      (cond
        (nil? next-point) seen

           ;; We have a candidate score and current score is greater. Skip the node.
        (and (not (nil? end-score)) (< end-score score)) (recur other-points seen)

           ;; Haven't seen point before
        (nil? exist-crumb) (recur (add-possibilities maze-map next-point other-points) (assoc seen current-point (Crumb. from-point score)))

        :else (if (> (.score exist-crumb) score)
                   ;; Score is better
                (recur (add-possibilities maze-map next-point other-points) (assoc seen current-point (Crumb. from-point score)))

                   ;; Score is no better than current point
                (recur other-points seen))))))

(let [maze-map (read-maze "input")
      start-point (find-point maze-map \S)
      end-point (find-point maze-map \E)
      forwards-moves (move-reindeer maze-map start-point :east end-point)
      backwards-moves (move-reindeer maze-map end-point :south start-point)
      req-score (get-in forwards-moves [end-point :score])]
  (loop [[candidate-point & rem-points] (keys forwards-moves)
         tile-count 0]

    (if (nil? candidate-point)
      tile-count
      (let [f-crumb (get forwards-moves candidate-point)
            b-crumb (get backwards-moves candidate-point)]
        (cond
          (or (nil? f-crumb) (nil? b-crumb)) (recur rem-points tile-count)
          (>= req-score (+ (.score f-crumb) (.score b-crumb))) (recur rem-points (inc tile-count))
          :else (recur rem-points tile-count))))))
