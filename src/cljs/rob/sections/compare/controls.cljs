(ns rob.sections.compare.controls
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [rob.math :as math])
  (:use
    [rob.state :only [watch-section bind-states assume]]
    [rob.input :only [input]]
    [rob.slider :only [slider]]
    [rob.copy :only [copy format-percent format-currency format-dollars format-currency-3]]))

(defn controls [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [parameter (fn [c-key value]
                        (list [:div {:class "parameter"}
                               [:div {:class "name"} (str (copy c-key) ": ")]
                               [:div {:class "value"} (format-currency value false)]]
                              [:div {:class "clear"}]))
            total-down (math/down-payment (:value (:house data)))
            monthly-buy (math/monthly-payment (:value (:house data)))
            monthly-rent (:value (:rent data))]
        (html
          [:div {:class "control-container"}
           [:div {:class "control-section"}
            [:h3 (copy :buy)]
            [:div {:class "input-and-slider"}
             [:div {:class "input-top"}
              (om/build input (:house data ) {:init-state {:name (copy :home-price)}})]
             [:div {:class  "input-bottom"}
              (om/build slider (:house data ) {:init-state {:label-fn #(format-currency-3 % false)}})]
             ]
            [:div {:class "parameters"}
             (parameter :down-payment (math/house-down-payment (:value (:house data))))
             (parameter :welcome-tax (math/land-transfer (:value (:house data))))
             (parameter :closing-costs (math/closing-costs (:value (:house data))))
             [:div {:class "line"}]
             (parameter :mortgage-monthly-payment (math/monthly-mortgage-payment (:value (:house data))))
             (parameter :monthly-maintainance (/ (math/maintainance (:value (:house data))) 12))]]
           [:div {:class "control-section"}
            [:h3 (copy :rent)]
            [:div {:class "input-and-slider"}
             [:div {:class "input-top"}
              (om/build input (:rent data ) {:init-state {:name (copy :monthly-rent)}})]
             [:div {:class  "input-bottom"}
              (om/build slider (:rent data ) {:init-state {:label-fn #(format-dollars % false)}})]]]
           [:div {:class "clear"}]
           [:div {:class "monthly-cost"}
            [:h3 {:class "mobile"} (copy :monthly-payment)]
            [:div {:class ["buy" (if (< monthly-buy monthly-rent) "cheaper")]}
             (str
               (format-dollars total-down false)
               " " (copy :down)
               " + "
               (format-dollars monthly-buy false)
               (copy :per-month))]
            [:div {:class ["rent" (if (< monthly-rent monthly-buy) "cheaper")]}
             (format-dollars monthly-rent false)
             (copy :per-month)]
            [:div {:class "clear"}]
            [:div {:class "versus"}
             [:div {:class "vs"} "VS"] ]] ] )))))
