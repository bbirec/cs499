(ns cs499.server
  (:require [noir.server :as server]))

(server/load-views "src/cs499/views/")

(def handler (server/gen-handler))


(defn -main [& m]
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8081"))]
    (server/start port {:mode mode
                        :ns 'cs499})))

