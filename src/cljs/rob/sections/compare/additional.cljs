(ns rob.sections.compare.additional
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [rob.math :as math])
  (:use
    [rob.state :only [watch-section bind-states assume]]
    [rob.input :only [input]]
    [rob.slider :only [slider]]
    [rob.copy :only [copy format-percent format-currency format-dollars round-to-0 nbsp]]))



(defn input-and-slider [data k]

  (let [l-fn (case (:format (k data))
               :percent #(str nbsp (format-percent %))
               :dollars #(format-dollars %)
               #(str nbsp nbsp (round-to-0 %))) ]
   [:div {:class "input-and-slider"}
   [:div {:class "input-top"}
    (om/build input (k data) {:init-state {:name (copy k)}})]
   [:div {:class  "input-bottom"}
    (om/build slider (k data ) {:init-state
                                {:label-fn l-fn}})]]))

(defn additional [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (html
        [:div {:class "additional"}

         [:div {:class "param"}
          [:h3 (copy :mortgage-details)]
          [:div {:class "desc"} (copy :mortgage-description)]
          ]
         [:div {:class "param"}
          (input-and-slider data :mortgage-interest)
          (input-and-slider data :amortization)
          (input-and-slider data :closing-costs)
          (input-and-slider data :selling-closing-costs) ]

         [:div {:class "param"}
          [:h3 (copy :yearly-increases)]
          [:div {:class "desc"} (copy :yearly-description)]]
         [:div {:class "param"}
          (input-and-slider data :home-growth)
          (input-and-slider data :rent-growth)
          (input-and-slider data :rate-of-return)
          (input-and-slider data :inflation) ]


         [:div {:class "param"}
          [:h3 (copy :home-ownership-costs)]
          [:div {:class "desc"} (copy :ownership-description)]
          ]
         [:div {:class "param"}
          (input-and-slider data :maintainance-fees)
          (input-and-slider data :property-tax) ] ]))))
