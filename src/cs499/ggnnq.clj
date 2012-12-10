(ns cs499.ggnnq
  (:use cs499.util)
  (:import [cs499.util Point Pair])
  (:use [cs499.comb :only [greedy round-robin brute-force]])
  (:require [clojure.math.combinatorics :as comb]))

(defrecord Result [dist p qs]
  java.lang.Comparable
  (compareTo [this o]
    (let [c (compare (:dist this) (:dist o))]
     (if (not= c 0)
       c
       (if (= this o) 0 1)))))

(defn gen-result
  [& pairs]
  (assert (> (count pairs) 0)
          "Need at least one pair.")
  (let [point (:p (first pairs))]
    (assert (every? #(= point (:p %)) pairs)
            "All of p should be equal")
    (Result.
     (reduce + (map :dist pairs))
     point
     (map :q pairs))))

(defn result-<
  [r1 r2]
  (compare r1 r2))

(defn resize-sorted-set [size set]
  (apply sorted-set-by result-< (take size (seq set))))

(defn add-to-result [result-set k new-results]
  (swap! result-set
         #(resize-sorted-set k (reduce conj % new-results))))

(defn equal-result? [r1 r2]
  (every? true? (map #(= (:dist %1) (:dist %2)) r1 r2)))


(defn rac
  ([nearest-pairs col row]
     (nth (nth nearest-pairs col) row))
  ([nearest-pairs col]
     (nth nearest-pairs col)))


(defn k-combination
  "Getting top k combination of the given pairs."
  [algorithm k & pairs]
  (let [inputs (map #(map (fn [i d] (vector i (:dist d))) (range) %) pairs)
        result (:result (algorithm inputs k))
        indices (map
                 (fn [[d & points]]
                   (map first points))
                 result)]
    (map #(map (fn [i j] (nth (nth pairs i) j))
               (range)
               %)
         indices)))


(defn find-same-origin-pairs
  "Find all pairs starting from given p until it reaches to row. At least one, the closest pair, is returned."
  [pairs row pair]
  (let [found (filter #(= (:p %) (:p pair))
                      (take (+ row 1) pairs))]
    (if (empty? found)
      (take 1 (filter #(= (:p %) (:p pair)) pairs))
      found)))

(defn find-results
  [nearest-pairs row col k k-comb]
  (let [pair (rac nearest-pairs col row)
        remains (keep-except col nearest-pairs)
        founds (map #(find-same-origin-pairs % row pair) remains)
        possibles
        (apply k-combination k-comb k founds)]
    (map #(apply gen-result (insert-item % col pair))
         possibles)))


(defn threshold
  [nearest-pairs row col]
  (let [pair (rac nearest-pairs col row)
        remains (keep-except col nearest-pairs)
        first-pairs (map first remains)
        first-dists (map :dist first-pairs)]
    (+ (:dist pair)
       (reduce + first-dists))))

(defn find-threshold
  [nearest-pairs row]
  (let [ths (map #(threshold nearest-pairs row %)
                 (range (count nearest-pairs)))]
    (apply min ths)))


(defn ggnnq
  "GGNNQ"
  ([k k-comb nearest-pairs]
     (let [k-comb (cond (= k-comb :greedy) greedy
                        (= k-comb :round-robin) round-robin
                        (= k-comb :brute-force) brute-force
                        :default (assert nil "Unsupported k-comb"))
           threshold (atom 0)
           result-set (atom (sorted-set-by result-<))
           idx (atom 0)]
    
       (while (and (or (empty? @result-set)
                       (< (count @result-set) k)
                       (and
                        (= (count @result-set) k)
                        (< @threshold (apply max (map :dist @result-set)))))
                   (< @idx (count (first nearest-pairs))))
      
         ;; Find results from each query sets
         (let [result (mapcat #(find-results nearest-pairs @idx % k k-comb)
                              (range (count nearest-pairs)))]
           ;; Adding to the result set
           (add-to-result result-set k result))
      
         ;; Set threshold
         (reset! threshold (find-threshold nearest-pairs @idx))

         ;; Increment idx
         (swap! idx inc))
       {:result @result-set}))
  ([k k-comb points & query-sets]
     (ggnnq k k-comb (map #(nearest-pair-sort points %) query-sets))))


;; Brute force algorithm

(defn brute-force-one
  [p & query-sets]
  (let [q-points (apply comb/cartesian-product query-sets)]
    (map (fn [points]
           (Result. (reduce
                     +
                     (map #(dist (vals p) (vals %))
                          points))
                    p
                    points))
         q-points)))

(defn brute-force-ggnnq
  [k points & query-sets]
  (let [results (mapcat #(apply brute-force-one % query-sets) points)]
    {:result (apply sorted-set (take k (sort-by :dist < results)))}))


;; Test

(defn check-ggnnq [k ds cq]
  (let [p (gen-random-points ds)
        qs (map (fn [_] (gen-random-points ds))
                (range cq))]
    ;; Brute force algorithm
    #_(do
      (prn "Brute force")
      (time (apply brute-force-ggnnq k p qs)))

    (let [nps (gen-nearest-pair-sort p qs)
          ; Use the data to eval laziness
          _ (prn (str "Generated " (count nps) " nearest pairs"))

          _ (prn "Starting the greedy")
          gd (with-time (ggnnq k :greedy nps))
          _ (prn "Starting the round robin")
          rr (with-time (ggnnq k :round-robin nps))]
      {:greedy (:time gd) :rr (:time rr)})))

(defn test-avg-time [t qs ds k]
  (avg (map (fn [_] (check-ggnnq k ds qs))
            (range t))))

