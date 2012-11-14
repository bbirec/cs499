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

(defn get-points [data-set indices]
  (map #(let [points (nth data-set %1)]
          (if (< %2 (count points))
            (nth points %2)
            nil))
       (range)
       indices))

(defn next-min-index [data-set indices]
  (let [possible (filter #(< (second %) (count (nth data-set (first %))))
                  (map-indexed vector indices))
        points (map #(vector (first %)
                             (nth (nth data-set (first %)) (second %)))
                    possible)]
    (first (apply min-key #(second (second %)) points))))

;; Algorithm

(defn greedy [data-set k]
  (let [first-points (map first data-set)
        first-value (sum-value first-points)
        first-set (conj first-points first-value)
        comp #(< (first %1) (first %2))
        result-set (atom (sorted-set-by comp first-set))
        indices (atom (vec (take (count data-set) (repeat 1))))
        bound (map #(count %) data-set)
        threshold (atom first-value)
        add-to-result (fn [results]
                        (swap! result-set
                               #(reduce conj % results)))]

    
    (while (or (and (< (count @result-set) k)
                    (some #(< (first %) (second %))
                          (map vector @indices bound)))
               (and (= (count @result-set) k)
                    (< @threshold (first (last @result-set)))))
      (let [min-idx (next-min-index data-set @indices)
            current-points (get-points data-set
                                       (update-in (vec (map dec @indices))
                                                  [min-idx]
                                                  inc))]

        (prn (pr-str @indices))

        ;; Add to result set
        (let [p (nth current-points min-idx)
              others (map #(take %1 %2)
                          @indices
                          (keep-except min-idx data-set))]
          (add-to-result
           (map
            #(result-points (insert-item % min-idx p))
            (apply com/cartesian-product others))))

        ;; Update the threshold
        (reset! threshold (get-threshold first-points current-points))

        ;; Update idx
        (swap! indices
               #(assoc % min-idx (+ (nth % min-idx) 1)))))
    (take k @result-set)))


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

;; Test for group access, time, # of calc threshold
;; Variations : data size, k, # query set

(def d (gen-data-set 5 5 100))

(defn check-time [qs ds k]
  (let [d (gen-data-set qs ds 1000)]
    ;;(prn "Brute force")
    ;;(time (brute-force d k))
    (prn "Round robin")
    (time (round-robin d k))
    nil))

