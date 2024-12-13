(ns puzzle)

(require '[clojure.java.io :as io])

(defn read-machine-puzzles [input-file]
  (with-open [content (io/reader input-file)]
    (vec (partition 4 4 [""] (line-seq content)))))

(defn parse-param [param]
  (let [matcher (re-matcher #"\d+" param)]
    [(Long/parseLong (re-find matcher)) (Long/parseLong (re-find matcher))]))

(defn parse-games-list [games-list]
  (map parse-param (take 3 games-list)))

(defn run-game [[a-x a-y] [b-x b-y] [d-x d-y]]
  ; b is cheaper, so try opimize b 
  (loop [b-count (min (quot d-x b-x) (quot d-y b-y))]
    (let [dx-rem (- d-x (* b-count b-x))
          dy-rem (- d-y (* b-count b-y))
          dax-quot (quot dx-rem a-x)
          day-quot (quot dy-rem a-y)]
      (cond
        (> 0 b-count) nil

        ; Exact match!
        (and (= 0 (mod dx-rem a-x) (mod dy-rem a-y)) (= dax-quot day-quot)) [dax-quot b-count]

        :else (recur (dec b-count))))))

(defn find-cheaper [a-params b-params d-params]
  (let [[a1-count b1-count :as count1] (run-game a-params b-params d-params)]
    (if (nil? count1)
      nil
      (let [[b2-count a2-count] (run-game b-params a-params d-params)]
        (min (+ (* 3 a1-count) b1-count) (+ (* 3 a2-count) b2-count))))))

(defn solve1 [input-file]
  (let [game-lines (read-machine-puzzles input-file)
        games (map #(parse-games-list (vec %)) game-lines)
        req-tokens (map #(apply find-cheaper %) games)]
    (reduce (fn [acc elem] (if (nil? elem) acc (+ acc elem))) 0 req-tokens)))

; (solve1 "sample")
; (solve1 "input")

(defn solve2 [input-file]
  (let [game-lines (read-machine-puzzles input-file)
         parsed-games (map #(parse-games-list (vec %)) game-lines)
         games (map (fn [[a-params b-params [d-x d-y]]] [a-params b-params [(+ d-x 10000000000000) (+ d-y 10000000000000)]]) parsed-games)
         req-tokens (map #(apply find-cheaper %) games)]
     (reduce (fn [acc elem] (if (nil? elem) acc (+ acc elem))) 0 req-tokens)))

; (solve1 "input")
; (solve2 "sample")