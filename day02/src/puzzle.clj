(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.edn :as edn])
(require '[clojure.string :as string])
(require '[clojure.set :as set])

(defn req-add [acc line]
  (let [[left right] (map Integer/parseInt (string/split line #"\|"))]
    (merge-with set/union  acc {left #{right}})))

(defn read-reqs [content]
  (reduce req-add {} content))

(defn read-orders [content]
  (map #(edn/read-string (str "[" (string/replace % "," " ") "]")) content))

(defn order-correct? [req-map seen elem]
  (let [must-before (get req-map elem)]
    (if (= nil must-before)
      true
      (empty? (set/intersection seen must-before)))))

(defn test-order [req-map order]
  (loop [[next & remaining] order
         seen #{}]
    (if (= nil next)
      true
      (if (order-correct? req-map seen next)
        (recur remaining (conj seen next))
        false))))

(defn get-mid [coll]
  (let [idx (quot (count coll) 2)]
    (get coll idx)))

(defn solve1 [input-file]
  (with-open [content (io/reader input-file)]
    (let [[req-lines _ order-lines] (partition-by #(string/blank? %) (vec (line-seq content)))
          reqs (read-reqs req-lines)
          orders (read-orders order-lines)
          match-list (filter #(test-order reqs %) orders)]
      (reduce + (map get-mid match-list)))))

; (solve1 "sample")
(solve1 "input")

(defn sort-by-req [read-reqs coll]
  (vec (sort #(let [after-values (get read-reqs %1)]
                (if (= nil after-values) 0
                    (if (contains? after-values %2) -1 1))) coll)))

(defn score-incorrect-line [reqs coll]
  (let [ordered (sort-by-req reqs coll)]
    (if (= coll ordered) 0 (get-mid ordered))))

(defn score-incorrect [reqs order-data]
  (reduce + 0 (map #(score-incorrect-line reqs %) order-data)))

(defn solve2 [input-file]
  (with-open [content (io/reader input-file)]
    (let [[req-lines _ order-lines] (partition-by #(string/blank? %) (vec (line-seq content)))
          reqs (read-reqs req-lines)
          orders (read-orders order-lines)]
      (score-incorrect reqs orders))))

(solve2 "input")