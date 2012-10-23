(ns cs499.ggnnq
  (:require [clojure.math.numeric-tower :as math])
  (:require [clojure.math.combinatorics :as comb]))

(defn gen-random-set
  "Generate random point set."
  [n bound]
  (into #{}
        (take n (repeatedly
                 #(vector (rand-int bound) (rand-int bound))))))


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



;; Assume that all of points are distinguishable

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

(defn rac
  ([nearest-pairs col row]
     (nth (nth nearest-pairs col) row))
  ([nearest-pairs col]
     (nth nearest-pairs col)))

(defn find-pairs
  "Find all pairs starting from given p until it reaches to row. At least one, the closest pair, is returned."
  [pairs row p]
  (let [found (filter #(= (first %) p) (take (+ row 1) pairs))]
    (if (empty? found)
      (take 1 (filter #(= (first %) p) pairs))
      found)))

(defn make-result-form
  [pair col & rest]
  (let [p (first pair)
        q (second pair)
        qs (into [] (insert-item (map second rest) col q))
        d (+ (dist-pair pair )
             (reduce + (map dist-pair rest)))]
    [d p qs]))

(defn find-candidates
  [nearest-pairs row col]
  (let [pair (rac nearest-pairs col row)
        p (first pair)
        q (second pair)
        remains (keep-except col nearest-pairs)
        founds (map #(find-pairs % row p) remains)
        possibles (apply comb/cartesian-product founds)]
    #_(prn "Pair:" (pr-str pair) " Possibles:" (pr-str possibles))
    (map #(apply make-result-form pair col %) possibles)))


(defn new-result-set
  "Return the new result set(size=k) with the new item."
  [s k results]
  (apply sorted-set
         (take k (apply sorted-set-by
                        #(<= (first %1) (first %2))
                        (clojure.set/union s results)))))

(defn threshold
  [nearest-pairs row col]
  (let [pair (rac nearest-pairs col row)
        remains (keep-except col nearest-pairs)
        first-pairs (map first remains)
        first-dists (map #(dist-pair %) first-pairs)]
    (+ (dist-pair pair)
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
        result-set (atom #{})
        idx (atom 0)]
    #_(prn "NNP" (pr-str nearest-pairs))
    (while (and (or (empty? @result-set)
                    (< (count @result-set) k)
                    (and
                     (= (count @result-set) k)
                     (< @threshold (apply max (map first @result-set)))))
                (< @idx (count (first nearest-pairs))))
      (prn (str "Iteration : " @idx))
      
      (let [next-pairs (map #(nth % @idx) nearest-pairs)]

        #_(prn "Current pairs:" (pr-str next-pairs))
        
        ;; Scores
        (let [candidates (apply concat
                                (map #(find-candidates nearest-pairs @idx %)
                                     (range (count query-sets))))]

          #_(prn "Candidates:" (pr-str candidates))
          #_(if (some #{[62.23M [52 54] [[41 50] [66 38] [81 50]]]}
                    candidates)
            (prn (pr-str candidates)))
          
          ;; Adding to the final set
          (reset! result-set
                  (new-result-set @result-set
                                  k
                                  (apply sorted-set candidates)))
          #_(prn "Now result map has " (str (count @result-set)))

          #_(if (some #{[62.23M [52 54] [[41 50] [66 38] [81 50]]]}
                    candidates)
            (prn (pr-str @result-set)))

          
          ;; Set threshold
          (let [t (find-threshold nearest-pairs @idx)
                #_(apply min (map first candidates))
                #_(reduce + (map #(dist-pair %) next-pairs))]
            (prn (str "T:" t))
            (reset! threshold t)))
        
        ;; Increment idx
        (swap! idx inc)))
    @result-set))


;; Brute force algorithm

(defn brute-force-one
  [p & query-sets]
  (let [q-points (apply comb/cartesian-product query-sets)]
    (map (fn [points]
           (vector (reduce + (map #(dist-pair (vector p %))
                                  points))
                   p
                   (into []  points)))
         q-points)))

(defn brute-force-ggnnq
  [k points & query-sets]
  (let [results (apply concat
                        (map #(apply brute-force-one % query-sets)
                            points))]
    (apply sorted-set (take k (sort-by first < results)))))


;; Test

(defn check-result [r1 r2]
  (prn (pr-str r1))
  (prn (pr-str r2))
  (if (= r1 r2)
    true
    (do
      (prn (pr-str (clojure.set/difference r1 r2)))
      (prn (pr-str (clojure.set/difference r2 r1)))
      false)))


(def lp
  (atom
   #{[33 1] [31 0] [52 54] [20 57] [23 68] [25 15] [35 91] [71 64] [23 51] [2 64]}))
(def lq1
  (atom
   #{[31 66] [30 4] [41 50] [79 27] [63 11] [6 83] [31 16] [34 85] [17 36] [57 20]}))
(def lq2
  (atom
   #{[41 13] [7 75] [93 1] [95 3] [66 38] [59 6] [69 19] [91 83] [56 18] [90 88]}))
(def lq3
  (atom
   #{[77 45] [81 50] [76 46] [13 84] [55 32] [36 49] [36 19] [12 27] [70 22] [58 77]}))

(defn do-test
  ([k p-size q-count q-size]
     (let [p (gen-random-set p-size 100)
           q1 (gen-random-set q-size 100)
           q2 (gen-random-set q-size 100)
           q3 (gen-random-set q-size 100)
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
