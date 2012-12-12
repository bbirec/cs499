(ns cs499.views.demo
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]])
  (:use [cs499.ggnnq :only [ggnnq]])
  (:use [cs499.util :only [gen-random-points]]))




(defpage "/demo/ta" {}
  (html
   [:html
    [:div
     [:h1 "Demo - Using Threshold Algorithm"]
     [:form {:method "POST"}
      "# of points in P"
      [:input {:type "text" :value "10" :name "p-count"}] [:br]
      
      "# of points for each Q"
      [:input {:type "text" :value "10" :name "q-count"}] [:br]
      
      "# of Q"
      [:input {:type "text" :value "3" :name "q-size"}] [:br]

      "Top k"
      [:input {:type "text" :value "10" :name "k"}] [:br]
      
      [:input {:type "submit"}]
      ]]]))


(defpage [:post "/demo/ta"] {:keys [p-count q-count q-size k]}
  (let [p (gen-random-points (Integer/parseInt p-count))
        qs (map (fn [_] (gen-random-points (Integer/parseInt q-count)))
                (range (Integer/parseInt q-size)))
        results (apply ggnnq
                       (Integer/parseInt k)
                       :greedy
                       p
                       qs)]
    (html
     [:html
      [:h1 "Random P"]
      [:div (pr-str p)]
      [:h1 "Random Qs"]
      (for [q qs]
        [:div (pr-str q)])
      [:h1 "Results: Set of [distance p [q1 q2...]"]
      [:div (pr-str results)]
      ])))