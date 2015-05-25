(ns rob.sections.compare.table
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [goog.string :as gstring]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [rob.math :as math])
  (:use
    [rob.state :only [watch-section bind-states assume]]
    [rob.copy :only [copy format-percent format-currency format-dollars format-currency-3]]))


(defn year-table-row
  ([cp b r] (year-table-row cp b r false 0))
  ([cp b r h span]
   [:tr
    (if h [:td {:rowSpan (str span)} h])
    [:td cp]
    [:td b]
    [:td r]]))

(defn desciption [rent buy year]
  (let [num-app (case year 1 "st" 2 "nd" 3 "rd" "th")
        r-key (if (> rent buy ) :renting :buying)
        diff (if (> rent buy) (- rent buy) (- buy rent))
        ]
    (str (copy :by-the) " " year num-app " "
         (.toLowerCase (copy :year)) " "
         (.toLowerCase (copy r-key)) " "
         (copy :is) " "
         (format-dollars diff false) " "
         (copy :cheaper))))

(defn cost-after [year]
  (let [y-key (if (< 1 year ) :years :year) ]
    (str (copy :cost-after) " " year " " (.toLowerCase (copy y-key)))))


(defn year-table [buy rent]
  [:table {:class "year-table"}
   [:tbody
    [:tr [:td  ] [:td (copy :buying)] [:td (copy :renting)] ]
    (year-table-row (copy :initial)
                    (format-dollars (-> buy :initial) false) "-")
    (year-table-row (copy :recurring)
                    (format-dollars (-> buy :recurring) false)
                    (format-dollars (-> rent :recurring) false))
    (year-table-row (copy :opportunity)
                    (format-dollars (-> buy :opportunity) false)
                    (format-dollars (-> rent :opportunity) false))
    (year-table-row (copy :selling)
                    (format-dollars (-> buy :profits))
                    "-")
    (year-table-row (copy :total)
                    (format-dollars (-> buy :total) false)
                    (format-dollars (-> rent :total) false)) ]])

(def radius 80)

(defn get-svg []
  (-> js/d3 (.select "#mortgage-info")))


(defn create-elements []
  (let [svg (get-svg)
        has-elements (-> svg (.select ".slices"))]
    (if (.empty has-elements)
      (do
        (-> svg
            (.append "g")
            (.attr "transform" (str "translate( " radius "," radius  ")"))
            (.attr "class" "slices"))
        (-> svg
            (.append "text")
            (.text (copy :home-worth))
            (.attr "class" "home-worth")
            (.append "tspan")
            (.attr "class" "home-price"))))))


(def svg-arc
  (-> js/d3 .-svg (.arc)
      (.outerRadius (- radius 2))
      (.innerRadius (- radius 20))))


(def pie-layout
  (-> js/d3 .-layout (.pie)
      (.sort nil)
      (.value #(.-value %))))

(defn arc-tween [a]
  (this-as this
    (let [current (.-_current this)
          current (if-not current a current)
          i (-> js/d3 (.interpolate current a))]
      (fn [t]
        (svg-arc (i t))))))

(defn adjust-svg [data]
  (let [jsdata (clj->js [
                  {:value (:mortgage data) :ismortgage true}
                  {:value (- (:home-worth data) (:mortgage data)) :ismortgage false}] )
        slice (-> (get-svg) (.select ".slices") (.selectAll "path.slice")
                  (.data (pie-layout jsdata)))]
    (-> slice
        (.enter)
        (.insert "path")
        (.attr "class" #(str "slice " (if (-> % .-data .-ismortgage) "mortgage"))))
    (-> slice
        (.transition)
        (.attrTween "d", arc-tween))
    (-> slice (.exit) (.remove))
    (-> (get-svg) (.select ".home-worth")
        (.attr "dy" (str  (+  radius) "px"))
        (.attr "dx" (str radius  "px"))
        (.attr "text-anchor" "middle"))
    (-> (get-svg) (.select ".home-price")
        (.attr "dy" "1em")
        (.attr "x" (str radius "px"))
        (.text (format-dollars (:home-worth data) false))
        (.attr "text-anchor" "middle"))))

(defn mortgage-table [data]
  [:div {:class "mortgage-table-container"}
   [:svg {:id "mortgage-info"} ]
   [:div {:class "legend"}
    [:div {:class "item"}
     [:div {:class "td color"} (str (copy :mortgage) ":")]
     [:div {:class "td price"} (format-dollars (:mortgage data) false)]
     ]
    [:div {:class "item"}
     [:div {:class "td color owner"} (str (copy :owner-value) ":")]
     [:div {:class "td price"} (format-dollars (- (:home-worth data) (:mortgage data)) false)] ] ] ])

(defn table-row-selected [cursor data owner state]
  (let [[year d] data
        buy (:buying d)
        rent (:renting d)
        format-dash (fn [v] (if (zero? v) "-" (format-dollars v)))
        fst (if (= 0 year) "first" "") ]
    [:tr {:class ["sub-table" fst]}
     [:td {:colSpan "3"}
      [:div {:class "sub-table-container"
             :onClick #(om/update! cursor [:selected-year] nil) }
       [:div {:class "sub-table-year"} (desciption (-> buy :total) (-> rent :total) (inc year))]
       [:div {:class "left half"}
        [:div {:class "year-table-header"} (cost-after (inc year))]
        (year-table buy rent)]
       [:div {:class "right"}
        [:div {:class "year-svg-header"} ]
        (mortgage-table buy)]
       [:div {:class "clear"}] ]]]))


(defn table-row [cursor data owner state]
  (let [[year d] data
        buy (-> d :buying :total)
        rent (-> d :renting :total)
        selected (if (nil? (:selected-year cursor)) 1000 (:selected-year cursor))
        over-year (inc selected)
        under-year (dec selected)
        add-class (if (= year under-year) "under-year" "")
        add-class (if (= year over-year) "over-year" add-class)
        buying-cheaper (if (> rent buy) "buying-cheaper" "") ]
    [:tr {:class [add-class buying-cheaper]
          :onClick #(if (not= (:selected-year cursor) year)
                          (om/update! cursor [:selected-year] year)
                          (om/update! cursor [:selected-year] nil)) }
     [:td (str (inc year))]
     [:td (format-dollars buy false) ]
     [:td (format-dollars rent false)] ]))

(defn table [data owner]
  (reify
    om/IDidUpdate
    (did-update [_ prev _]
      (let [ data (get (:calcs data) (:selected-year data)) ]
        (if data
          (do (create-elements)
              (adjust-svg (:buying data))))))
    om/IRenderState
    (render-state [_ state]
      (html
        [:div {:class "table-container"}
         [:table {:class "future-table"}
          [:tbody
           [:tr {:class "header"}
            [:td (copy :year)]
            [:td (copy :buying)]
            [:td (copy :renting)] ]
           (map #(if (= (get % 0) (:selected-year data))
                   (table-row-selected data % owner state)
                   (table-row data % owner state))
                (butlast (:calcs data)))]]]))))
