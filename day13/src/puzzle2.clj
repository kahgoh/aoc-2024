(ns puzzle2)

(require '[clojure.java.io :as io])

(defn read-machine-puzzles [input-file]
  (with-open [content (io/reader input-file)]
    (vec (partition 4 4 [""] (line-seq content)))))

(defn parse-param [param]
  (let [matcher (re-matcher #"\d+" param)]
    [(Long/parseLong (re-find matcher)) (Long/parseLong (re-find matcher))]))

(defn parse-games-list [games-list]
  (map parse-param (take 3 games-list)))

(defn test-a-game [[a-x a-y] [b-x b-y] [d-x d-y] av]
  ; b is cheaper, so try opimize b 
  (loop [a-count (inc av)]
    (let [dx-rem (- d-x (* a-count a-x))
          dy-rem (- d-y (* a-count a-y))
          dax-quot (quot dx-rem b-x)
          day-quot (quot dy-rem b-y)]
      (cond
        (> (dec av) a-count) nil

        ; Exact match!
        (and (= 0 (mod dx-rem b-x) (mod dy-rem b-y)) (= dax-quot day-quot)) (+ (* a-count 3) dax-quot)

        :else (recur (dec a-count))))))

(defn solve-game [[ax ay :as a-params] [bx by :as b-params] [dx dy :as d-params]]
  (let [rha (- dx (/ (* bx dy) by))
        lha (- ax (/ (* bx ay) by))
        av (long (/ rha lha))] 
      (test-a-game a-params b-params d-params av)))

(defn solve1 [input-file]
  (let [game-lines (read-machine-puzzles input-file)
        games (map #(parse-games-list (vec %)) game-lines)
        req-tokens (map #(apply solve-game %) games)]
    (reduce (fn [acc elem] (if (nil? elem) acc (+ acc elem))) 0 req-tokens)))

(defn solve2 [input-file]
  (let [game-lines (read-machine-puzzles input-file)
         parsed-games (map #(parse-games-list (vec %)) game-lines)
         games (map (fn [[a-params b-params [d-x d-y]]] [a-params b-params [(+ d-x 10000000000000) (+ d-y 10000000000000)]]) parsed-games)
         req-tokens (map #(apply solve-game %) games)]
     (reduce (fn [acc elem] (if (nil? elem) acc (+ acc elem))) 0 req-tokens)))



(solve2 "input")