(ns cs499.util
  (:require [clojure.math.numeric-tower :as math]))

(defn keep-except
  "Keep sequence except the given indexed item."
  [idx coll]
  (keep-indexed #(if-not (= idx %1) %2) coll))


(defn insert-item
  "Insert an item into coll at idx"
  [coll idx item]
  (concat (take idx coll)
          (list item)
          (take-last (- (count coll) idx) coll)))


(defn min-index [coll]
  (first (apply min-key second (map-indexed vector coll))))



(defn round-places
  "Round the float number to have the given number of decimals"
  [number decimals]
  (let [factor (math/expt 10 decimals)]
    (bigdec (/ (math/round (* factor number)) factor))))

(defn dist
  "Euclidean distance between two points."
  [p1 p2]
  (round-places (math/sqrt (reduce +
                                   (map #(math/expt (- %1 %2) 2) p1 p2)))
                2))

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



(defn first-<
  [r1 r2]
  (let [c (compare (first r1) (first r2))]
    (if (not= c 0)
      c
      (compare r1 r2))))

(defn resize-sorted-set [size set]
  (apply sorted-set-by first-< (take size (seq set))))

(defn add-to-result [result-set k new-results]
  (swap! result-set
         #(resize-sorted-set k (reduce conj % new-results))))


(defn equal-result? [r1 r2]
  (every? true? (map #(= (first %1) (first %2)) r1 r2)))
