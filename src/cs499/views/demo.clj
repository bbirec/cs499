(ns cs499.views.demo
  (:use [noir.core :only [defpage defpartial render]]
        [noir.response :only [redirect]]
        [hiccup.core :only [html]])
  (:use [cs499.ggnnq :only [ggnnq]]
        [cs499.util :only [with-time]])
  (:use [clojure.math.numeric-tower :only [expt]])
  (:import [cs499.util Point Pair])
  (:use [cs499.util :only [gen-random-points gen-nearest-pair-sort]]))


(defn html- [head body]
  (html
   [:html
    [:head
     [:link {:rel "stylesheet" :href "/css/bootstrap.min.css"}]
     [:link {:rel "stylesheet" :href "http://code.jquery.com/ui/1.9.2/themes/base/jquery-ui.css"}]
     [:script {:src "http://code.jquery.com/jquery-1.8.3.js"}]
     [:script {:src "http://code.jquery.com/ui/1.9.2/jquery-ui.js"}]
     head]
    [:body
     body]]))

(defn form-input [title name value]
  [:div {:class "control-group"}
   [:label {:class "control-label"} title]
   [:div {:class "controls"}
    [:input {:type "text" :name name :value value}]]])

(defn form-submit [title]
  [:div {:class "form-actions"}
   [:button {:type "submit" :class "btn btn-primary"} title]])

(defn form [title opt & body]
  [:form opt
   [:legend title]
   body])


(defonce p (atom nil))
(defonce qs (atom nil))
(defonce nps (atom nil))


(defpage "/demo/data" {}
  (html-
   (list)
   [:div {:class "container"}
    [:div {:class "well"}
     (form
      "랜덤 POI 데이터 생성" {:method "POST"}
      (form-input "Data size(m)" :m "100")
      (form-input "# of Query types(n)" :n "3")
      (form-submit "생성"))]]))


(defpage [:post "/demo/data"] {:keys [m n]}
  (try
    (let [m (Integer/parseInt m)
          n (Integer/parseInt n)]
      (reset! p (gen-random-points m))
      (reset! qs (map (fn [_] (gen-random-points m))
                      (range n)))
      (reset! nps (gen-nearest-pair-sort @p @qs))
      (redirect "/demo/ta"))
    (catch Exception e (redirect "/demo/data"))))


(defpartial data-status []
  [:div
   [:h4 "Data Status"]
   [:ul
    [:li (str "# of points in P : " (count @p))]
    [:li (str "# of points in Q : " (reduce str
                                            (interpose ", "(map #(str (count %)) @qs))))]]])

(defn nps-count
  ([]
     (apply min (map count @nps)))
  ([max-count]
     (min max-count (nps-count))))


(defn str-p [p]
  (str "(" (:x p) ", " (:y p) ")"))

(defn str-pair [pair]
  (str "[" (str-p (:p pair)) " " (str-p (:q pair))))

(defpartial nearest-pair-table []
  [:h2 "Closest Pairs"]
  [:ul [:li (str "Total # of rows : " (nps-count))]]
  [:table {:class "table table-bordered table-striped table-hover"}
   
   [:thead
    [:th "Idx"]
    (for [title (map #(str "P-Q" %)
                     (range (count @nps)))]
      (list [:th title]
            [:th "Distance"]))]
   
   [:tbody
    (for [i (range (nps-count 5))]
      [:tr
       [:td (str i)]
       (for [j (range (count @nps))]
         (let [pair (nth (nth @nps j) i)]
          (list
           [:td (str-pair pair)]
           [:td (str (:dist pair))])))])]])


(defpartial result-table [{result :result time :time}]
  [:h2 "Result"]
  [:ul
   [:li (str "k = " (count result))]
   [:li (str "Elapsed Time : " time " ms")]]

  [:table {:class "table table-bordered table-striped table-hover"}
   [:thead
    [:th "Idx"]
    [:th "Distance"]
    [:th "P"]
    (for [title (map #(str "Q" %)
                     (range (count @qs)))]
      [:th title])]
   
   [:tbody
    (for [[i r] (map #(vector %1 %2) (range) result)]
      [:tr
       [:td (str i)]
       [:td (str (:dist r))]
       [:td (str-p (:p r))]
       (for [q (:qs r)]
         [:td (str-p q)])])]])

(defpartial search-form []
  [:h2"Search"]
  [:ul [:li (str "Total # of candidates : " (expt (nps-count) (count @qs)))]]
  [:form {:method "POST"}
      [:div
       [:input {:type "text" :class "input-medium search-query" :placeholder "Top-k" :name :k}]
       [:button {:type "submit" :class "btn"} "검색"]]])

(defpage "/demo/ta" {:keys [results] :as param}
  (html-
   (list)
   [:div {:class "container"}
    [:h1 "과제연구 MGNNQ Demo"]
    [:blockquote
     [:p "주어진 point set P와 n개의 point set Q 중에서 각각의 와의 거리의 합이 가장 작은 k개의 p를 효율적으로 찾는 방법"]]

    [:div
     [:h2 "Sample Data"]
     (data-status)
     [:a {:href "/demo/data"} "New random data set"]]

    ;; Nearest pair sort table
    (nearest-pair-table)

    ;; Search form
    (search-form)

    ;; Result table
    (if-not (empty? results)
     (result-table results))]))


(defpage [:post "/demo/ta"] {:keys [k]}
  (let [k (try (Integer/parseInt k) (catch Exception e 0))]
    (if (> k 0)
      (render "/demo/ta" {:results (with-time (ggnnq k :greedy @nps))})
      (render "/demo/ta"))))