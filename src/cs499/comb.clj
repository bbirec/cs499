(ns cs499.comb
  (:use [cs499.util])
  (:require [clojure.math.combinatorics :as com]))

;; Sorted combination

(defn gen-data-set
  "Generate the sorted data"
  [num-of-query data-size bound]
  (map (fn [_]
         (let [data (take data-size (repeatedly #(rand-int bound)))
               sorted (sort < data)]
           (map #(vector %1 %2)
                (range data-size)
                sorted)))
       (range num-of-query)))

(defn sum-value [points]
  (reduce + (map second points)))

(defn get-threshold [first-points current-points]
  (apply min
         (map sum-value
              (map (fn [i]
                     (conj
                      (keep-except i first-points)
                      (nth current-points i)))
                   (range (count first-points))))))

(defn result-points [points]
  (conj points (sum-value points)))

;; Algorithm

(defn greedy [data-set k]
  )


(defn round-robin [data-set k]
  (let [first-points (map first data-set)
        first-value (sum-value first-points)
        first-set (conj first-points first-value)
        comp #(< (first %1) (first %2))
        result-set (atom (sorted-set-by comp first-set))
        idx (atom 1)
        threshold (atom first-value)
        add-to-result (fn [results]
                        (swap! result-set
                               #(reduce conj % results)))]
    
    (while (or (and (< (count @result-set) k)
                    (< @idx (apply max (map count data-set))))
               (and (= (count @result-set) k)
                    (< @threshold (first (last @result-set)))))
      (let [current-points (map (fn [d]
                                  (if (< @idx (count d))
                                    (nth d @idx)
                                    (last d))) ; Return last item
                                data-set)]
        
        ;; Add to result set
        (dotimes [i (count data-set)]
          (let [p (nth current-points i)
                 others (map #(take (+ @idx 1) %)
                             (keep-except i data-set))]
             (add-to-result
              (map
               #(result-points (insert-item % i p))
               (apply com/cartesian-product others)))))
        
        ;; Update the threshold
        (reset! threshold (get-threshold first-points current-points)))

      ;; Inc idx
      (swap! idx inc))
    (take k @result-set)))



(defn brute-force [data-set k]
  (let [products (apply com/cartesian-product data-set)
        results (map result-points products)]
    (take k (sort-by first < results))))

;; Test for group access, time
;; Variations : data size, k, # of calc threshold, # query set



