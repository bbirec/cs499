(ns cs499.factual
  (:use [factql.core])
  (:require [clojure.math.numeric-tower :as math])
  (:require [factual.api :as fact])
  (:require [monger.collection :as mc]))



(def auth-key (get (System/getenv) "FACTUAL_KEY" ""))
(def auth-secret (get (System/getenv) "FACTUAL_SECRET" ""))


(fact/factual! auth-key auth-secret)



(defn get-restaurants []
  (select restaurants-us
          (around {:lat 34.06021 :lon -118.4183 :miles 3})
          (search "cigar")
          (where
           (= :alcohol true)
           (= :meal_dinner true)
           (= :parking_free true))))


(defn total-count [result]
  (get-in (meta result) [:response :total_row_count]))

(defn included-count [result]
  (get-in (meta result) [:response :included_rows]))

;; Fetch functions

(defn find-starbucks [o l]
  (let [result (fact/fetch {:table :global
                            :include_count true
                            :q "스타벅스"

                            :limit l})]
    result))

(defn find-func-by-name [name]
  (fn [o l] (let [result (fact/fetch {:table :global
                                      :include_count true
                                      :q name
                                      :filters "{\"country\":\"kr\"}"
                                      :offset o
                                      :limit l})]
     result)))

;; Fetch and Save

(defn coll-data [result]
  {:name (:name result)
   :location {:lat (:latitude result)
              :lon (:longitude result)}})

(defn fetch-and-save [f o l coll]
  (let [req (f o l)
        rows (included-count req)
        data (map coll-data req)]
    (try
      (prn (str "Fetched " rows " rows from " o))
      (mc/insert-batch coll data)
      (prn (str "Stored " rows))
      (catch Exception e
        (prn (str "Failed to store" e))))
    (+ o l)))


(def max-fetch-size 50)

(defn count-f [f]
  (total-count (f 0 1)))

(defn fetch-all [f coll]
  (let [total (count-f f)]
    (prn (str "Total " total " found."))
    (let [fetches (range (math/ceil (/ total max-fetch-size)))
          offsets (map #(* % max-fetch-size) fetches)
          futures (map #(future (fetch-and-save f % max-fetch-size coll))
                       offsets)
          results (map #(deref %) futures)]
      results)))


(defn fetch-all-by-name [name coll]
  (fetch-all (find-func-by-name name) coll))

;; Collections
;; http://www.factual.com/data/t/global


(def coll-cafe "cafe-coll")
(def cafe-names ["스타벅스" "카페베네" "엔제리너스" "탐앤탐스" "커피"])

(def coll-hotel "hotel-coll")
(def hotel-name ["호텔" "모텔" "여관"])

(def coll-store "store-coll")
(def coll-fastfood "fastfood-coll")
(def coll-movie "coll-movie")
(def coll-department "coll-department")