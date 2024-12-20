(ns puzzle)

(require '[clojure.java.io :as io])
(require '[clojure.string :as strings])

(defn read-input [input-file]
  (with-open [reader (io/reader input-file)]
    (vec (partition-by strings/blank? (line-seq reader)))))

; Part 1
(defn solve1 [input-file]
  (let [[[available] _ desired] (read-input input-file)
        regex (strings/replace available #",\s" "|")
        pattern (re-pattern (str "(" regex ")+"))]
    (count (filter #(re-matches pattern %) desired))))

;; (solve1 "input")
(defn count-routes
  ([desired choices prev-counts]
   (loop [[next-choice & rem-choices] choices
          counter 0]
     (cond
       (nil? next-choice) counter

       ;; Exact match on the string
       (= next-choice desired) (recur rem-choices (inc counter))

       ;; Doesn't end with the current choice
       (not (strings/ends-with? desired next-choice)) (recur rem-choices counter)
       
       ;; Choice is too long
       (> (count next-choice) (count desired)) (recur rem-choices counter)

       :else (let [prefix (subs desired 0 (- (count desired) (count next-choice)))
                   prefix-count (get prev-counts prefix)]

               (if (nil? prefix-count)

                 ;; No route to the prefix.
                 (recur rem-choices counter)

                 ;; Can reach the prefix
                 (recur rem-choices (+ counter prefix-count)))))))

  ([desired choices]
   (loop [test-length 1
          counts {}]
     (if (> test-length (count desired))
      ;; Finished
       (get counts desired)

       ;; Keep going
       (let [test-string (subs desired 0 test-length)
             test-string-count (count-routes test-string choices counts)]
         (recur (inc test-length) (assoc counts test-string test-string-count)))))))

(defn solve2 [input-file]
  (let [[[available] _ desired] (read-input input-file)
        choices (mapv strings/trim (strings/split available #","))]
    (reduce + (map #(count-routes % choices) desired))))


;; (solve2 "sample")
(solve2 "input")

;; (def choices (let [[[available] _ desired] (puzzle/read-input "input")] (mapv strings/trim (strings/split available #","))))
;; (count-routes "rwuugrgubgwgbrrurggwbugbuuwgbugbuwwwubbwbbw" choices)
