(ns cs499.comb
  (:use [clojure.tools.trace])
  (:use [cs499.util])
  (:import [cs499.util Point])
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
  (vec (conj points (sum-value points))))

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
        result-set (atom (sorted-set-by first-<
                                        (vec first-set)))
        indices (atom (vec (take (count data-set) (repeat 1))))
        bound (map #(count %) data-set)
        threshold (atom first-value)
        count-threshold (atom 0)
        count-access (atom 0)]
    
    (while (and (some #(< (first %) (second %))
                      (map vector @indices bound))
                (or (< (count @result-set) k)
                    (and (>= (count @result-set) k)
                     (<= @threshold (first (nth (seq @result-set) (dec k)))))))
      (let [min-idx (next-min-index data-set @indices)
            current-points (get-points data-set
                                       (update-in (vec (map dec @indices))
                                                  [min-idx]
                                                  inc))]

        ;; Add to result set
        (let [p (nth current-points min-idx)
              others (map #(take %1 %2)
                          (keep-except min-idx @indices)
                          (keep-except min-idx data-set))
              results (map
                       #(result-points (insert-item % min-idx p))
                       (apply com/cartesian-product others))]
          (add-to-result result-set k results)
          ;; Update the counter
          (swap! count-access #(+ % (count results))))

        ;; Update the threshold
        (reset! threshold (get-threshold first-points current-points))

        ;; Update the counter
        (swap! count-threshold inc)

        
        ;; Update idx
        (swap! indices
               #(assoc % min-idx (+ (nth % min-idx) 1)))))
    
    {:count-threshold @count-threshold
     :count-access @count-access
     :result @result-set}))


(defn round-robin [data-set k]
  (let [first-points (map first data-set)
        first-value (sum-value first-points)
        first-set (conj first-points first-value)
        result-set (atom (sorted-set-by first-<
                                        (vec first-set)))
        idx (atom 1)
        threshold (atom first-value)
        count-threshold (atom 0)
        count-access (atom 0)]
    
    (while (and (< @idx (apply max (map count data-set)))
                (or (< (count @result-set) k)
                    (and (>= (count @result-set) k)
                         (<= @threshold (first (nth (seq @result-set) (dec k)))))))
      (let [current-points (map (fn [d]
                                  (if (< @idx (count d))
                                    (nth d @idx)
                                    (last d))) ; Return last item
                                data-set)]
        
        ;; Add to result set
        (dotimes [i (count data-set)]
          (let [p (nth current-points i)
                others (map #(take (+ @idx 1) %)
                            (keep-except i data-set))
                results (map
                         #(result-points (insert-item % i p))
                         (apply com/cartesian-product others))]
            (add-to-result result-set k results)

            ;; Update the counter
            (swap! count-access #(+ % (count results)))))
        
        ;; Update the threshold
        (reset! threshold (get-threshold first-points current-points))
        ;; Update the counter
        (swap! count-threshold inc))


      ;; Inc idx
      (swap! idx inc))
    {:count-threshold @count-threshold
     :count-access @count-access
     :result @result-set}))



(defn brute-force [data-set k]
  (let [products (apply com/cartesian-product data-set)
        results (map result-points products)]
    {:count-threshold 0
     :count-access 0
     :result (take k (sort-by first < results))}))



;; Test for group access, time, # of calc threshold
;; Variations : data size, k, # query set

(defonce d (gen-data-set 5 5 100))
(defonce fd (atom nil))

(defn prn-result [result]
  (prn (str "Access count: " (get result :count-access)))
  (prn (str "Threshold count: " (get result :count-threshold)))
  (prn (str "Elapsed time: " (get result :time)))
  #_(doseq [r (seq (get result :result))]
    (prn (pr-str r))))


(defmacro with-time [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (merge ret# {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)})))

(defn test-avg-time [func t qs ds k]
  (let [data (map (fn [_] (gen-data-set qs ds 10000)) (range t))
        results (map (fn [d] (dissoc (with-time (func d k)) :result)) data)
        avg (into {} (for [[k v] (apply merge-with + results)]
               [k (double (/ v (count results)))]))]
    avg))


(defn check-time
  ([d k]
     (let [rr (with-time (round-robin d k))
           gd (with-time (greedy d k))]
       (if (equal-result? rr gd)
         (do (prn "Round robin")
             (prn-result rr)
             (prn "Greedy")
             (prn-result gd)             
             true)
         (do
           (prn "Round Robin")
           (prn-result rr)
           (prn "Greedy")
           (prn-result gd)
           (reset! fd d)
           false))))
  ([qs ds k]
     (check-time (gen-data-set qs ds 10000) k)))

(defn compare-algorithms
  ([t qs ds k]
     (list (test-avg-time round-robin t qs ds k)
           (test-avg-time greedy t qs ds k))))

