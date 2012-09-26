(ns cs499.views.welcome
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]]))


(defpage "/" []
  (html
   [:h1 "CS499 과제연구 (Fall 2012)"]
   [:div
    [:p "20051294 문희홍"]]
   
   [:h2 "Description"]
   

   [:h2 "Progress"]
   [:ul
    [:li "~ 2012. 9. 18 : 주제 정하기 위한 첫번째 미팅"]
    [:li "~ 2012. 9. 24 : R-tree, SSQ(Spatial Skyline Quries)에 대한 논문 읽어보기"]]
   
   [:h2 "References"]
   [:ul
    [:li "R-TREES: A dynamic index structure for spatial searching"]
    [:li "The Spatial Skyline Queries"]]
    
   [:h2 "Links"]
   [:div
    [:ul
     [:li [:a {:href "http://www.postech.ac.kr/class/cs499/2012fall/"
               :target "_blank"}
           "CSED499 Class Homepage"]]]]
   [:h2 "Report and Papers"]
   [:h2 "Demo"]))

