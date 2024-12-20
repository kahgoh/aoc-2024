(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.string :as strings])

(def dir-offsets [[-1 0] [0 1] [1 0] [0 -1]])

(defn read-maze [input-file]
  (let [content (io/reader input-file)]
    (mapv vec (line-seq content))))

(defn find-index [coll item]
  (loop [idx 0]
    (let [elem (get coll idx)]
      (cond
        (nil? elem) nil
        (= elem item) idx
        :else (recur (inc idx))))))

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

(defn get-adjacent-spaces [maze-map at-point distance-map]
  (let [adjacent-spaces (mapv #(mapv + at-point %) dir-offsets)
        filtered (filter (fn [test-point] (and (not (contains? distance-map test-point)) (not= \# (get-in maze-map test-point)))) adjacent-spaces)]
    (vec filtered)))

(defn calculate-path [maze-map start-point end-point]
  (loop [[next-point & rem-points] [start-point]
         next-queue []
         tiles {}
         depth 0]

    (cond
      ;; Current queue processed and no more items to go through
      (and (nil? next-point) (empty? next-queue)) tiles

      ;; Current queue processed, but more levels to go through
      (nil? next-point) (recur next-queue [] tiles (inc depth))

      ;; Already seen.
      (contains? tiles next-point) (recur rem-points next-queue tiles depth)

      ;; Reached the end (also wouldn't be in the tiles map)
      (= next-point end-point) (assoc tiles next-point depth)

      ;; Haven't seen the tile before
      :else (recur rem-points (into next-queue (get-adjacent-spaces maze-map next-point tiles)) (assoc tiles next-point depth) depth))))

(defn get-next-path [tiles from]
  (let [score (get tiles from)
        candidates (mapv #(mapv + from %) dir-offsets)
        filtered (filter #(= (get tiles % -1) (dec score)) candidates)]
    (first filtered)))

(defn reverse-trace [tiles start]
  (loop [current start
         acc []]
    (let [score (get tiles current)
          next-acc (conj acc current)]
      (cond
        (= 0 score) next-acc
        :else (recur (get-next-path tiles current) (conj acc current))))))

(def shortcut-dirs [[-2 0] [0 2] [2 0] [0 -2]])

(defn test-shortcut [tiles allowable-tiles from to shortcuts]
  (let [from-dist (get tiles from)
        to-dist (get tiles to)]
    (if (or (not (contains? allowable-tiles to)) (nil? to-dist))
      ;; To point is either prohibited or unknown distance
      shortcuts

      (let [shortcut-dist (- to-dist from-dist 2)]
        (if (<= shortcut-dist 0)
          ;; To point goes backwards or doesn't offer any advantage
          shortcuts

          (update shortcuts shortcut-dist #(if (nil? %) 1 (inc %))))))))

(defn get-shortcuts [tiles allowable-tiles from shortcuts]
  (reduce (fn [acc dir-offset]
            (let [test-point (mapv + from dir-offset)]
              (test-shortcut tiles allowable-tiles from test-point acc)))
          shortcuts
          shortcut-dirs))

(defn find-shortcuts [tiles reverse-path]
  (let [allowable-tiles (into #{} reverse-path)]
    (reduce (fn [acc point] (get-shortcuts tiles allowable-tiles point acc)) {} reverse-path)))

(defn solve1 [input-file]
  (let [maze-map (read-maze input-file)
        start-point (find-point maze-map \S)
        end-point (find-point maze-map \E)
        tiles (calculate-path maze-map start-point end-point)
        shortcuts (into [] (find-shortcuts tiles (reverse-trace tiles end-point)))]
    (reduce (fn [acc [time count]] (if (< time 100) acc (+ acc count))) 0 shortcuts)))
