(ns rob.sections.compare
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [clojure.string :as string]
            [sablono.core :as html :refer-macros [html]]
            [rob.math :as math])
  (:use
    [rob.state :only [watch-section bind-states assume]]
    [rob.copy :only [copy copy-p format-percent format-currency format-dollars format-currency-3]]
    [rob.dropdown :only [dropdown]]
    [rob.input :only [input]]
    [rob.slider :only [slider]]
    [rob.section :only [change-section validate-section get-section set-section!]]
    [rob.sections.compare.controls :only [controls]]
    [rob.sections.compare.additional :only [additional]]
    [rob.sections.compare.table :only [table]]
    [rob.sections.compare.graph :only [graph]]))

(defn with-previous [old new]
  (reduce
    (fn [m [k v]]
      (if
        (< (get-in m [k :min] 0) (:value v) (get-in m [k :max] 0))
        (assoc-in m [k :value] (:value v))
        m))
    new old))

(defn make-data [old]
  (with-previous old
   (let [max-rent (math/max-rent)
         min-rent 100
         ideal-rent (math/ideal-rent)
         ideal-rent (if (> ideal-rent max-rent) max-rent
                      (if (< ideal-rent min-rent) min-rent ideal-rent))
         max-home-price (math/max-home-price)
         min-home-price 10000
         ideal-home-price (math/ideal-home-price)
         ideal-home-price (if (> ideal-home-price max-home-price) max-home-price
                      (if (< ideal-home-price min-home-price)
                        min-home-price ideal-home-price))]
    {:house {:format :dollars :min min-home-price :max max-home-price
             :value ideal-home-price :ticks 5}
    :rent {:format :dollars :min min-rent :max max-rent
           :value ideal-rent :ticks 5}})))


(defmethod validate-section :compare [section]
  (let [current (get-section :compare)
        data (make-data (:input current)) ]
    (set-section! :compare
      (assoc current :valid true :input data))))



(defmethod watch-section :compare [_ state old-state]
  (if (and (or
             (not= (:assumptions state) (:assumptions old-state))
             (not= (:input state) (:input old-state))) (:valid state))
    (do
      (assoc-in state [:input :calcs]
                (math/calc-assets (get-in state [:input :house :value])
                                  (get-in state [:input :rent :value]))))))

(defn describe-future [data]
  (let [calcs (:calcs data)
        comp-total #(if (< (-> % :buying :total) (-> % :renting :total)) :buying :renting)
        cheapers (map comp-total (vals calcs))
        indexed (first (filter (fn [[i v]] (not= v (first cheapers))) (map-indexed vector cheapers)))
        initial (first cheapers) ]
    (if indexed
      (str (copy (second indexed)) " "
           (copy :is-cheaper-after) " "
           (first indexed) " "
           (if (= 1 (inc (first indexed)))
             (.toLowerCase (copy :year))
             (.toLowerCase (copy :years))))
      (str (copy initial) " "
           (copy :is-cheaper-for-next) " "
           (dec (assume :years)) " " (copy :years)))))

(defn replace-desc [copy years buy rent]
  (-> copy
       (string/replace #"XX" (str (dec years)))
       (string/replace #"YY" (format-currency-3 buy false))
       (string/replace #"ZZ" (format-dollars rent false))) )

(defn compare-comp [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [card (get-section :people :people)]
        (html
         [:section {:class ["compare"
                            (if (:current data) "active")]}
          [:div {:class "subsection"}
           [:h2 {:class "no-top"} (copy :compare-title)]
           [:div {:class "description"} (copy-p :compare-desc)]
           (if (:input data) (om/build controls (:input data)))
           [:h2 (if (:input data) (describe-future (:input data)))]
           [:div {:class "description"}
            (if (:input data) (replace-desc (copy :buying-desc)
                          (assume :years)
                          (-> data :input :house :value)
                          (-> data :input :rent :value)))]
           [:div {:class "ib half top non-mobile"} [:h3 (copy :yearly-table)] ]
           [:div {:class "ib half too non-mobile"}
              [:h3 (copy :avg-monthly-graph)]
              [:table {:class "leg"}
               [:tbody
               [:tr
                [:td {:class "rent color"}]
                [:td (copy :renting-is-cheaper)]
                [:td {:class "buy color"}]
                [:td (copy :buying-is-cheaper)]]]]]
           [:div {:class "clear"}]
           [:div {:class "yearly-container"}
            (if (:input data) (om/build table (:input data)))
            (if (:input data) (om/build graph (:input data)))]
           [:h2 (copy :additional-parameters)]
           [:div {:class "description"} (copy-p :assumptions-desc)]
           (if (:assumptions data) (om/build additional (:assumptions data)))

           [:div {:class "social"}
            [:div {:class "social-button facebook"}
             [:div {:class "fb-like"
                    :data-href "http://filipwolanski.com/rent-or-buy/"
                    :data-layout "button"
                    :data-action "like"
                    :data-show-faces "false"
                    :data-share "true" }]]
             [:div {:class "social-button twitter"}
              [:a {:class "twitter-share-button"
                   :href "https://twitter.com/share"
                   :data-count  "none"
                   :data-url="http://filipwolanski.com/rent-or-buy/"
                   }]
              ]
             [:div {:class "social-button github"}
              [:iframe {:src "https://ghbtns.com/github-btn.html?user=fwolanski&repo=filipwolanski.com&type=star"
                        :frameBorder "0"
                        :scrolling "0"
                        :width "170px"
                        :height "20px"}]]]
           [:div {:class "clear"}]

            [:div {:class "social-description"}
             "This app is build with Clojurescript and "
             [:a {:href "https://github.com/omcljs/om" :target "_blank"} "om"] ", a thin wrapper over "
             [:a {:href "https://facebook.github.io/react/" :target "_blank"} "Facebook's React"]
             ". The source is available "
             [:a {:href "https://github.com/fwolanski/rent-or-buy/" :target "_blank"} "here"]
             " on Github." ] ] ])))))
