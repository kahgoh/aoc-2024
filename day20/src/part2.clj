(ns part2)

(require '[puzzle])

(defn test-shortcut [tiles allowable-tiles max-dist from to shortcuts]
  (let [man-dist (reduce + (mapv #(abs (- %1 %2)) from to))
        from-time (get tiles from)
        to-time (get tiles to)]
    (if (or (not (contains? allowable-tiles to)) (> man-dist max-dist) (nil? to-time))
      ;; To point is either prohibited, too far or unknown distance
      shortcuts

      (let [shortcut-dist (- to-time from-time man-dist)]
        (if (<= shortcut-dist 0)
          ;; To point goes backwards or doesn't offer any advantage
          shortcuts

          (update shortcuts shortcut-dist #(if (nil? %) 1 (inc %))))))))

(defn get-shortcuts [tiles allowable-tiles max-dist from shortcuts]
  (reduce (fn [acc test-point]
            (test-shortcut tiles allowable-tiles max-dist from test-point acc))
          shortcuts
          allowable-tiles))

(defn find-shortcuts [tiles reverse-path max-dist]
  (let [allowable-tiles (into #{} reverse-path)]
    (reduce (fn [acc point] (get-shortcuts tiles allowable-tiles max-dist point acc)) {} reverse-path)))

(defn solve2 [input-file]
  (let [maze-map (puzzle/read-maze input-file)
        start-point (puzzle/find-point maze-map \S)
        end-point (puzzle/find-point maze-map \E)
        tiles (puzzle/calculate-path maze-map start-point end-point)
        shortcuts (into [] (find-shortcuts tiles (puzzle/reverse-trace tiles end-point) 20))]
    (reduce (fn [acc [time count]] (if (< time 100) acc (+ acc count))) 0 shortcuts)))

    ;; Sorted times
    ;; (sort (into [] shortcuts))))

(solve2 "input")
