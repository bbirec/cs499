(ns cs499.views.welcome
  (:use [noir.core :only [defpage]]
        [hiccup.core :only [html]]))


(defn link [name link]
  [:a {:href link :target "_blank"} name])

(defpage "/" []
  (html
   [:h1 "CS499 과제연구 (Fall 2012)"]
   [:div
    [:p "20051294 문희홍"]
    [:p "Advisor : 황승원 교수님"]]
   
   [:h2 "Topic Description"]
   [:div
    [:p "주어진 point set P와 n개의 point set Qi(1<=i<=n)중에서 각각의 Qi와의 거리의 합이 가장 작은 k개의 p를 효율적으로 찾는 방법을 연구한다."]
    ]

   [:h2 "Demo"]
   [:div
    [:ul
     [:li (link "TA방식 구현" "/demo/ta")]
     [:li (link "위치 Data" "/viewer")]]]

   [:h2 "Report and Papers"]
   [:div
    [:ul
     [:li (link "Proposal" "https://docs.google.com/document/pub?id=1XvMMiIomaZMbtb9r_okDsAiY-Y9K0uKs66lI6I0oIJ0")]]]


   [:h2 "Progress"]
   [:ul
    [:li "~ 2012. 9. 18 : 주제 정하기 위한 첫번째 미팅"]
    [:li "~ 2012. 9. 24 : R-tree, SSQ(Spatial Skyline Quries)에 대한 논문 읽어보기"]
    [:li "~ 2012. 10. 4 : Threshold Algorithm, Group Nearest Neighbor Queries 에 대한 논문 Reading, Proposal 작성 준비"]
    [:li "~ 2012. 10. 9 : Data crawling(from factual.com), Topic formal하게 정해보기, Baseline이 될 algorithm pseudo code 작성해 보기"]
    [:li "~ 2012. 10. 16 : Bruteforce방식으로 구현, 알고리즘 구현해서 비교해보기."]
    [:li "~ 2012. 10. 23 : Threshold Algorithm방식으로 구현, Bruteforce방식과 비교"]]

   
   
   [:h2 "References"]
   [:ul
    [:li "R-TREES: A dynamic index structure for spatial searching"]
    [:li "The Spatial Skyline Queries"]
    [:li "Best Position Algorithms for Top-k Queries"]
    [:li "Group Nearest Neighbor Queries"]
    [:li (link "Java Spatial Index : R-tree java implementation" "https://github.com/aled/jsi")]
    [:li (link "Factual" "http://factual.com")]]
    
   [:h2 "Links"]
   [:div
    [:ul
     [:li (link "CSED499 Class Homepage" "http://www.postech.ac.kr/class/cs499/2012fall/")]
     [:li (link "This Web Site Source Code" "https://bitbucket.org/bbirec/cs499")]]]
   [:hr]
   [:p "Hosted on Heroku. Written in Clojure."]))

