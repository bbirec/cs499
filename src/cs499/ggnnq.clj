(ns cs499.ggnnq
  (:use cs499.util)
  (:import [cs499.util Point])
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


(defn find-same-origin-pairs
  "Find all pairs starting from given p until it reaches to row. At least one, the closest pair, is returned."
  [pairs row pair]
  (let [found (filter #(= (:p %) (:p pair))
                      (take (+ row 1) pairs))]
    (if (empty? found)
      (take 1 (filter #(= (:p %) (:p pair)) pairs))
      found)))

(defn find-candidates
  [nearest-pairs row col]
  (let [pair (rac nearest-pairs col row)
        remains (keep-except col nearest-pairs)
        founds (map #(find-same-origin-pairs % row pair) remains)
        possibles (apply comb/cartesian-product founds)]
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
  [k points & query-sets]
  (let [nearest-pairs (map #(nearest-pair-sort points %) query-sets)
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
      (let [result (mapcat #(find-candidates nearest-pairs @idx %)
                           (range (count query-sets)))]
        ;; Adding to the result set
        (add-to-result result-set k result))
      
      ;; Set threshold
      (reset! threshold (find-threshold nearest-pairs @idx))

      ;; Increment idx
      (swap! idx inc))
    @result-set))


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
    (apply sorted-set (take k (sort-by :dist < results)))))


;; Test

(defonce fr1 (atom nil))
(defonce fr2 (atom nil))

(defn check-result [r1 r2]
  #_(prn (pr-str r1))
  #_(prn (pr-str r2))
  (if (equal-result? r1 r2)
    true
    (do
      (reset! fr1 r1)
      (reset! fr2 r2)
      (prn (pr-str (clojure.set/difference r1 r2)))
      (prn (pr-str (clojure.set/difference r2 r1)))
      false)))


(defonce lp
  (atom
   #{[33 1] [31 0] [52 54] [20 57] [23 68] [25 15] [35 91] [71 64] [23 51] [2 64]}))
(defonce lq1
  (atom
   #{[31 66] [30 4] [41 50] [79 27] [63 11] [6 83] [31 16] [34 85] [17 36] [57 20]}))
(defonce lq2
  (atom
   #{[41 13] [7 75] [93 1] [95 3] [66 38] [59 6] [69 19] [91 83] [56 18] [90 88]}))
(defonce lq3
  (atom
   #{[77 45] [81 50] [76 46] [13 84] [55 32] [36 49] [36 19] [12 27] [70 22] [58 77]}))

(defn check-ggnnq [k np cq nq]
  (let [p (gen-random-points np 10000)
        qs (map (fn [_] (gen-random-points nq 10000))
                (range cq))]
    (time (apply ggnnq k p qs))
    nil))

(defn do-test
  ([k p-size q-count q-size]
     (let [p (gen-random-points p-size 100)
           q1 (gen-random-points q-size 100)
           q2 (gen-random-points q-size 100)
           q3 (gen-random-points q-size 100)
           r1 (brute-force-ggnnq k p q1 q2 q3)
           r2 (ggnnq k p q1 q2 q3)]
       (reset! lp p)
       (reset! lq1 q1)
       (reset! lq2 q2)
       (reset! lq3 q3)
       (check-result r1 r2))))


(defn do-test2
  ([k P & Q]
     (let [r1 (apply brute-force-ggnnq k P Q)
           r2 (apply ggnnq k P Q)]
       (check-result r1 r2))))

(defn do-test3
  [imp k p-size q-size]
  (let [p (gen-random-points p-size 100)
        q1 (gen-random-points q-size 100)
        q2 (gen-random-points q-size 100)
        q3 (gen-random-points q-size 100)
        r (time (imp k p q1 q2 q3))]
    (reset! lp p)
    (reset! lq1 q1)
    (reset! lq2 q2)
    (reset! lq3 q3)
    r))