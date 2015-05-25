(ns rob.sections.compare.graph
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [goog.dom :as gdom]
            [rob.math :as math])
  (:use
    [rob.state :only [watch-section bind-states assume]]
    [rob.copy :only [copy format-percent format-currency format-dollars format-currency-3]]))

(def plot-key :monthly-average)
(def hide-base true)
(def selected-year-height (- 290 24))
(def interpolation "monotone")
(def transition-duration 600)


(defn get-svg []
  (-> js/d3 (.select "#graph")))

(defn get-axis []
  (-> js/d3 (.select "#graph") (.select ".axis")))

(defn get-buy-group [tp]
  (-> js/d3 (.select "#graph") (.select (str ".buy-" tp))))

(defn get-rent-group [tp]
  (-> js/d3 (.select "#graph") (.select (str ".rent-" tp))))

(defn get-clipping-group [tp]
  (-> js/d3 (.select "#graph") (.select (str "." tp "-clipping-path"))))

(defn create-elements [data]
  (let [svg (get-svg)
        group-fn (fn [cls tp]
                   (-> svg
                       (.append "g")
                       (.attr "class" cls)
                       (.attr "transform" "translate(0,24)")
                       (.append "path")
                       (.attr "class" (str cls "-" tp))
                       ))
        ]
    (let [defs (-> svg (.append "defs"))]
      (-> defs
          (.append "clipPath")
          (.attr "id" "buy-clipping")
          (.append "path")
          (.attr "class" "buy-clipping-path"))
      (-> defs
          (.append "clipPath")
          (.attr "id" "rent-clipping")
          (.append "path")
          (.attr "class" "rent-clipping-path")))
    (-> svg
        (.append "g")
        (.attr "class" "axis")
        (.attr "transform" "translate(0,24)"))
    (-> (group-fn "buy" "area") (.attr "clip-path" "url(#buy-clipping)"))
    (-> (group-fn "rent" "area") (.attr "clip-path" "url(#rent-clipping)"))
    (group-fn "buy" "line")
    (group-fn "rent" "line")))


(defn year-height [obj]
  (let [def-height (if (< (.-selected obj) (.-year obj)) selected-year-height 0)]
   (+ (* (inc (.-year obj)) 24) def-height)))

(defn svg-area [scale]
  (-> js/d3 .-svg (.area)
      (.y year-height)
      (.x0 0)
      (.x1 #(scale (.-total %)))
      (.interpolate interpolation)))

(defn invert-svg-area [scale mx]
  (-> js/d3 .-svg (.area)
      (.y year-height)
      (.x1 (scale mx))
      (.x0 #(scale (.-total %)))
      (.interpolate interpolation)))

(defn svg-line [scale]
  (-> js/d3 .-svg (.line)
      (.y year-height)
      (.x #(scale (.-total %)))
      (.interpolate interpolation)))

(defn update-group [group-fn data scale anim]
  (if anim
    (do
      (-> (group-fn "area")  (.datum (clj->js data))
          (.transition) (.duration transition-duration) (.attr "d" (svg-area scale)))
      (-> (group-fn "line")  (.datum (clj->js data))
          (.transition) (.duration transition-duration) (.attr "d" (svg-line scale))) )
    (do
      (-> (group-fn "area")  (.datum (clj->js data)) (.attr "d" (svg-area scale)))
      (-> (group-fn "line")  (.datum (clj->js data)) (.attr "d" (svg-line scale))) )))

(defn clip-group [rent buy scale mx anim]
  (if anim
    (do
      (-> (get-clipping-group "buy")  (.datum (clj->js rent))
          (.transition) (.duration transition-duration) (.attr "d" (invert-svg-area scale mx)))
      (-> (get-clipping-group "rent") (.datum (clj->js buy))
          (.transition) (.duration transition-duration) (.attr "d" (invert-svg-area scale mx))))
    (do
      (-> (get-clipping-group "buy")  (.datum (clj->js rent)) (.attr "d" (invert-svg-area scale mx)))
      (-> (get-clipping-group "rent") (.datum (clj->js buy)) (.attr "d" (invert-svg-area scale mx))) )))

(defn tick-format [d]
  (if (zero? d) "" (format-dollars d false)))

(def svg-scale
  (memoize
    (fn []
      (let [width (.-width (.getBoundingClientRect (.node (get-svg))))]
        (-> js/d3 .-scale (.linear)
            (.range #js [0 width]))))))

(def svg-axis
  (memoize
    (fn [scale]
      (-> js/d3 .-svg (.axis) (.scale scale) (.orient "top") (.ticks 5)
          (.tickFormat tick-format)))))


(defn get-min-max [data]
  (let [buy-totals (map (fn [[k v]] (-> v :buying plot-key)) data )
        rent-totals (map (fn [[k v]] (-> v :renting plot-key)) data )
        mins (* 0.9 (min (apply min buy-totals) (apply min rent-totals)))
        maxs (* 1.1 (max (apply max buy-totals) (apply max rent-totals)))]
    [0 maxs]))

(defn map-prices [cursor data tp]
   (let [selected (if (:selected-year cursor) (dec (:selected-year cursor)) 1000)]
    (map (fn [[k v]] (hash-map :year (dec k) :total (-> v tp plot-key) :selected selected)) data)))


(defn adjust-svg [cursor data anim]
  (let [d-buy (map-prices cursor data :buying)
        d-rent (map-prices cursor data :renting)
        [mi mx] (get-min-max data)
        scale (-> (svg-scale) (.domain #js [mi mx]))]
    (-> (get-axis) (.call (svg-axis scale)))
    (update-group get-buy-group d-buy scale anim)
    (update-group get-rent-group d-rent scale anim)
    (clip-group d-rent d-buy scale mx anim)))

(defn adjust-container [data]
  (let [has-selected-year (if (-> data :selected-year) true false)
        container (gdom/getElementByClass "graph-container")]
    (aset (.-style container) "height" (str (if has-selected-year 890 624) "px"))) )

(defn graph [data owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (let [calcs (:calcs data)]
        (create-elements data)
        (adjust-svg data calcs false)
        (adjust-container data)))
    om/IDidUpdate
    (did-update [_ prev _]
      (let [anim (if (and
                        (= (-> prev :house :value) (-> data :house :value))
                        (= (-> prev :rent :value) (-> data :rent :value))
                        (not= (-> prev :selected-year) (-> data :selected-year)))
                    true false)
             calcs (:calcs data)]
        (adjust-svg data calcs anim)
        (adjust-container data)))
    om/IRenderState
    (render-state [_ state]
      (html
        [:div {:class "graph-container"}
         [:svg {:id "graph"}]]))))
