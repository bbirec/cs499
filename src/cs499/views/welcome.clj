(ns cs499.views.welcome
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]]))


(defpage "/" []
  (html
   [:h1 "CS499 과제연구 (Fall 2012)"]
   [:div
    [:p "20051294 문희홍"]]
   
   [:h2 "Topic Description"]
   [:div]

   [:h2 "Progress"]
   [:ul
    [:li "~ 2012. 9. 18 : 주제 정하기 위한 첫번째 미팅"]
    [:li "~ 2012. 9. 24 : R-tree, SSQ(Spatial Skyline Quries)에 대한 논문 읽어보기"]
    [:li "~ 2012. 9. 4 : Threshold Algorithm, Group Nearest Neighbor Queries 에 대한 논문 Reading, Proposal 작성 준비"]]
   
   [:h2 "References"]
   [:ul
    [:li "R-TREES: A dynamic index structure for spatial searching"]
    [:li "The Spatial Skyline Queries"]
    [:li "Best Position Algorithms for Top-k Queries"]
    [:li "Group Nearest Neighbor Queries"]
    [:li [:a {:href "https://github.com/aled/jsi"} "Java Spatial Index : R-tree java implementation"]]]
    
   [:h2 "Links"]
   [:div
    [:ul
     [:li [:a {:href "http://www.postech.ac.kr/class/cs499/2012fall/"
               :target "_blank"}
           "CSED499 Class Homepage"]]
     [:li [:a {:href "https://bitbucket.org/bbirec/cs499"
               :target "_blank"}
           "This Web Site Project"]]]]
   [:h2 "Report and Papers"]
   [:h2 "Demo"]))

