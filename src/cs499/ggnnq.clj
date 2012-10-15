(ns cs499.ggnnq
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as comb]))

(defn gen-random-set
  "Generate random point set."
  [n bound]
  (into #{}
        (take n (repeatedly
                 #(vector (rand-int bound) (rand-int bound))))))



(defn dist
  "Euclidean distance between two points."
  [p1 p2]
  (math/sqrt (reduce + (map #(math/expt (- %1 %2) 2) p1 p2))))

(defn dist-pair
  "Distance of point pair"
  [pair]
  (reduce dist pair))





(defn nearest-sort
  "Return sorted query set Q by distances from the given point p."
  [p Q]
  (sort (fn [q1 q2] (< (dist p q1)
                       (dist p q2)))
        Q))

(defn nearest-point
  "Find the nearest point q from p"
  [p Q]
  (first (nearest-sort p Q)))




(defn nearest-pair-sort
  "Return pair set sorted by the distance between two point in each set."
  [P Q]
  (sort (fn [pair1 pair2] (< (dist-pair pair1)
                             (dist-pair pair2)))
        (for [p P q Q] (vector p q))))



;; Assume that all of points are distinguishable

(defn keep-except
  "Keep sequence except the given indexed item."
  [idx coll]
  (keep-indexed #(if-not (= idx %1) %2) coll))


(defn calc-score
  "Calculating the score. The idx is the index of query set which has the pair."
  [pair idx query-sets]
  (let [p (first pair)
        qs (keep-except idx query-sets)
        closest-pair (map #(vector p (nearest-point p %1)) qs)
        closest-dist (map dist-pair closest-pair)]
    [(+ (dist-pair pair) (reduce + closest-dist))
     pair
     closest-pair]))

(defn min-index [coll]
  (first (apply min-key second (map-indexed vector coll))))



(defn ggnnq
  "GGNNQ"
  [k points & query-sets]
  (let [nearest-pairs (map #(nearest-pair-sort points %) query-sets)
        threshold (atom 0)
        best-dist (atom 99999)
        idx (atom 0)]
    (while (and (< @threshold @best-dist)
                (< @idx 100))
      
      (let [next-pairs (map #(nth % @idx) nearest-pairs)]
        ;; Set threshold

        ;; Scores
        (let [results (map #(calc-score %1 %2 query-sets)
                          next-pairs
                          (range))
              scores (map first results)
              min-score-idx (min-index scores)
              min-score (nth scores min-score-idx)
              min-score-pair (nth next-pairs min-score-idx)
              ]
          (prn (str (nth results min-score-idx)))
          ;(prn "Found pair:" min-score-pair " score=" min-score)
          )
        
        ;; Increment idx
        (swap! idx inc)))))


;; Brute force algorithm

(defn brute-force-one
  [p & query-sets]
  (let [q-points (apply comb/cartesian-product query-sets)]
    (map (fn [points]
           (vector (reduce + (map #(dist-pair (vector p %))
                                  points))
                   p points))
         q-points)))

(defn brute-force-ggnnq
  [k points & query-sets]
  (let [results (apply concat
                        (map #(apply brute-force-one % query-sets)
                            points))]
    (take k (sort-by first < results))))
