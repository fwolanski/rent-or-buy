(ns rob.slider
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require
            [cljs.core.async :refer [put! chan <! mult untap tap sliding-buffer]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [goog.string :as gstring]
            [goog.dom :as gdom]
            [goog.style :as gstyle]
            [goog.string.format]
            [goog.dom.classes :as classes])
  (:use [rob.events :only [fast-click movement]]
        [rob.copy :only [format-percent]]))

(defn nodelist-to-seq [nl]
  (let [result-seq (map #(.item nl %) (range (.-length nl)))]
    (doall result-seq)))

(defn data-range [data]
  (- (:max data) (:min data)))


(defn handle-dimensions [data owner el-width p]
  (let [t-dist-left (* el-width p)
        t-dist-right (- el-width t-dist-left)
        measure (gdom/getElementByClass "scale-handle-measure" (om/get-node owner))
        width (.-width (.getBoundingClientRect measure))
        mid (/ width 2)
        margin (if (< t-dist-left mid) t-dist-left
                 (if (< t-dist-right mid) (+ mid (- mid t-dist-right)) mid))
        margin (if (:no-move data) t-dist-left margin)]
    {:width width :margin margin}))

(defn adjust-handle-text [data owner el-width p]
  (let [dimensions (handle-dimensions data owner el-width p)
        root-el (om/get-node owner)
        handle-text (gdom/getElementByClass "scale-handle-text" root-el)
        scale-items (nodelist-to-seq (gdom/getElementsByClass "scale-item" root-el))
        tick-pos (map #(* %  (/ el-width (dec (:ticks data)))) (range (:ticks data)))
        hidden-range [(- (* p el-width) (:margin dimensions))
                      (+ (* p el-width)
                         (- (:width dimensions) (:margin dimensions)))]
        hide-margin 16]
    (aset (.-style handle-text) "width" (str (:width dimensions) "px"))
    (aset (.-style handle-text) "marginLeft" (str (- (:margin dimensions)) "px"))
    (let [[min max] hidden-range]
      (doseq [[i e] (map-indexed vector scale-items)]
        (if (< (- min hide-margin) (nth tick-pos i) (+ max hide-margin))
          (classes/add e "covered")
          (classes/remove e "covered"))))))

(defn adjust-handle [data owner]
  (let [root-el (om/get-node owner)
        width (.-width (.getBoundingClientRect root-el))
        handle-el (gdom/getElementByClass "slider-handle" root-el)
        p (* 100 (/ (- (:value data) (:min data)) (data-range data)))
        diff (/ (- width 16) width)
        adj (* p diff)]
    (aset (.-style handle-el) "left" (str adj "%"))
    (adjust-handle-text data owner width (/ adj 100))))

(defn slider-tracks [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:active false})
    om/IRenderState
    (render-state [_ state]
      (let [hide (:disabled data)]
        (let [ticks (for [k (range (:ticks data))]
                      (+ (:min data) (* k (/ (data-range data) (- (:ticks data) 1)))))]
         (html
          [:div {:class "slider-scale"}
            (for [x ticks]
              (let [v (* 100 (/ (- x (:min data)) (data-range data)))]
                [:div {:class ["scale-item" (if hide "hidden")]
                       :style {:left (str v "%")} }
                 [:span {:class "scale-item-text"} ((:label-fn state) x)]]))]))))))



(defn slider [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:movement (chan)
       :active false})
    om/IWillMount
    (will-mount [_]
      (let [action (om/get-state owner :movement)]
        (go (loop []
              (let [start (<! action)
                    rect (.getBoundingClientRect (om/get-node owner))
                    left (.-left rect)
                    right (.-right rect)
                    p (/ (- start left) (- right left))
                    p (if (< p 0) 0 p)
                    p (if (> p 1) 1 p)
                    data (om/get-props owner) ]
                (if (not= :end start)
                  (let [v (+ (:min data) (* (data-range data) p))]
                    (om/update! data :value v))
                    (do
                      (untap movement action)
                      (om/set-state! owner :active false)))
                (recur))))))
    om/IDidMount
    (did-mount [_]
      (if (not (false? (om/get-state owner :label-fn)))
        (adjust-handle data owner)))
    om/IDidUpdate
    (did-update [_ _ _]
      (if (not (false? (om/get-state owner :label-fn)))
        (adjust-handle data owner)))
    om/IRenderState
    (render-state [_ state]
      (let [p (* 100 (/ (- (:value data) (:min data)) (data-range data)))
            l (- 100 p)]
       (html
        [:div {:class ["slider"
                       (if (:active state) "active")
                       (if (:disabled data) "disabled")
                       (if (:nolabel data) "nolabel")]}
          [:div {:class "slider-handle"
                 :onMouseDown #(do
                                 (.blur (.-activeElement js/document))
                                 (.preventDefault %)
                                 (om/set-state! owner :active true)
                                 (tap movement (:movement state)))
                 :onTouchStart #(do
                                  (.blur (.-activeElement js/document))
                                  (.preventDefault %)
                                  (om/set-state! owner :active true)
                                  (tap movement (:movement state)))}
           (if (not (false? (:label-fn state)))
             [:div {:class "scale-handle-text" }
              [:span {:class "scale-handle-measure"} (if (:label-fn state)
                                                       ((:label-fn state) (:value data))
                                                       (format-percent p 1))]])]
          [:div {:class "slider-from" :style {:width (str p "%")}}]
          [:div {:class "slider-to" :style {:width (str l "%")}}]
          (om/build slider-tracks data {:init-state {:label-fn (:label-fn state)}})])))))
