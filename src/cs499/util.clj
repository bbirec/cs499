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



;; Data types

(defrecord Point [x y]
  java.lang.Comparable
  (compareTo [this o] (and (compare (:x this) (:x o))
                           (compare (:y this) (:y o)))))

(defrecord Pair [dist p q])

(defrecord Result [dist p qs]
  java.lang.Comparable
  (compareTo [this o] (compare (:dist this) (:dist o))))

(defn gen-random-points
  "Generate N random points with number bound."
  [n bound]
  (map (fn [_] (Point. (rand-int bound) (rand-int bound)))
       (range n)))

(defn gen-pair
  "Making a point pair with the distance between two points."
  [p q]
  (Pair. (dist (vals p) (vals q)) p q))


(defn nearest-pair-sort
  "Return pair set sorted by the distance between two point in each set. [dist pair]"
  [P Q]
  (sort-by
   :dist
   (for [p P q Q]
     (gen-pair p q))))


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
  (let [c (compare (:dist r1) (:dist r2))]
    (if (not= c 0)
      c
      (compare r1 r2))))

(defn resize-sorted-set [size set]
  (apply sorted-set-by result-< (take size (seq set))))

(defn add-to-result [result-set k new-results]
  (swap! result-set
         #(resize-sorted-set k (reduce conj % new-results))))

(defn equal-result? [r1 r2]
  (every? true? (map #(= (:dist %1) (:dist %2)) r1 r2)))




