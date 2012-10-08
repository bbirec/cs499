(ns cs499.views.dataviewer
  (:require [monger.collection :as mc])
  (:use [cs499.common :only [collections]])
  (:use [noir.core :only [defpage defpartial]]
        [hiccup.core :only [html]]))

(defn coll-from-name [name]
  (get collections (keyword name)))


(defpartial coll-list []
  [:ul
   (map (fn [n]
          [:li [:a {:href (str "/viewer/" n)} n]])
        (map name (keys collections)))])

(defpage "/viewer" []
  (html
   [:div
    [:h1 "Geo data viewer"]
    [:p "Data source:"
     [:a {:href "http://www.factual.com/data/t/global"
          :target "_blank"}
      "http://www.factual.com/data/t/global"]]
    (coll-list)]))


(defpartial point-list [coll]
  [:p "Total :" (str (mc/count coll)) " rows"]
  [:table
   [:tr [:td "이름"] [:td "위치"]]
   (map (fn [d]
          [:tr
           [:td (str (get d :name))]
           [:td (str (get d :location))]])
        (mc/find-maps coll))])

(defpage "/viewer/:coll" {:keys [coll]}
  
  (if (some #{coll} (map name (keys collections)))
    (point-list (coll-from-name coll))
    "Invalid coll"))

