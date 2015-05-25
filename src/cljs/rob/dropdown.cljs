(ns rob.dropdown
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require
            [cljs.core.async :refer [put! chan <! tap untap]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [goog.string :as gstring]
            [goog.string.format]
            )
  (:use [rob.events :only [fast-click down]])
  )

(defn deactivate [owner]
  (untap down (om/get-state owner :down))
  (om/set-state! owner :active false)
  )

(defn activate [owner]
  (tap down (om/get-state owner :down))
  (om/set-state! owner :active true)
  )

(defn dropdown [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:active false
       :down (chan)})
    om/IWillMount
    (will-mount [_]
    (let [active (om/get-state owner :down )]
      (go (while true
            (let [deact (<! active)]
              (deactivate owner))))))
    om/IRenderState
    (render-state [_ state]
      (html
        [:div {:class ["dropdown"  (if (:active state ) "active")]}
          [:div (merge {:class "selected"} (fast-click
                                             #(do
                                                (.blur (.-activeElement js/document))
                                                (if (om/get-state owner :active)
                                                  (deactivate owner)
                                                  (activate owner)))))
             (str ((:value data) (:values state)))]
          [:div {:class "bar"}]
          [:div {:class ["options" ]}
           (map (fn [[k v]] [:div
            (merge {:class "option" :key k}
             (fast-click #(do
                            (om/update! data [:value] k)
                            (deactivate owner))))
            (str v)]) (:values state))]]))))
