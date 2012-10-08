(ns cs499.server
  (:require [noir.server :as server])
  (:require [monger.core :as mg]))

(server/load-views "src/cs499/views/")

(def handler (server/gen-handler))

(def mongo-uri (get (System/getenv) "CS499_MONGO_URI" ""))
(defn connect []
  (let [uri (get (System/getenv)
                   "MONGOLAB_URI" mongo-uri)]
      (prn "Connecting to hosted db")
      (mg/connect-via-uri! uri)))



(defn -main [& m]
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8081"))]
    (connect)
    (server/start port {:mode mode
                        :ns 'cs499})))

