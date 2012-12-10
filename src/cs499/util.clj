(ns cs499.util
  (:use [clojure.java.io :only [writer]])
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

;;

(defmacro with-time [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (merge ret# {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)})))

(defn avg
  "Calculate the avg for map values"
  [coll]
  (into {} (for [[k v] (apply merge-with + coll)]
             [k (double (/ v (count coll)))])))

;; Data types

(defrecord Point [x y]
  java.lang.Comparable
  (compareTo [this o] (and (compare (:x this) (:x o))
                           (compare (:y this) (:y o)))))

(defrecord Pair [dist p q]
  java.lang.Comparable
  (compareTo [this o] (compare (:dist this) (:dist o))))

(def default-random-max 10000)

(defn gen-random-point
  ([]
     (gen-random-point default-random-max))
  ([bound]
     (Point. (rand-int bound) (rand-int bound))))

(defn gen-random-points
  "Generate N random points with number bound."
  ([n]
     (gen-random-points n default-random-max))
  ([n bound]
     (map (fn [_] (gen-random-point bound))
          (range n))))

(defn gen-pair
  "Making a point pair with the distance between two points."
  [^Point p ^Point q]
  (Pair. (dist (vals p) (vals q)) p q))


(defn nearest-pair-sort
  "Return pair set sorted by the distance between two point in each set. [dist pair]"
  [P Q]
  #_(apply sorted-set (for [^Point p P ^Point q Q]
                        (gen-pair p q)))
  #_(let [result (atom (sorted-set))]
    (for [p P q Q]
      (swap! result #(conj %1 (gen-pair p q)))))
  (sort-by
   :dist
   (for [p P q Q]
     (gen-pair p q))))

(defn gen-nearest-pair-sort [p qs]
  (map #(nearest-pair-sort p %) qs))

