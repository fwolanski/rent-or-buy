(ns rob.section
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [goog.events :as events]
            [goog.events.EventType :as EventType])
  (:use [rob.events :only [fast-click]]
        [rob.state :only [app-state]]
        [rob.copy :only [copy]]))


(defn set-section! [& args]
  (let [section (vec (butlast args))
        data (last args)]
   (swap! app-state assoc-in section data)))

(defn get-section [& args]
  (let [section (vec args)]
   (get-in @app-state section)))

(defn section-name []
  (->> @app-state
       (filter #(:current (second %)))
       first
       first
       name
       ))

(defn make-from-cardinalty [k people form]
  (let [i (if (empty? k) [] k)
        missing (- people (count i))]
    (cond
      (neg? missing) (vec (drop-last (- missing) i))
      (pos? missing) (apply conj i (repeat missing form))
      :else i)))

(defmulti validate-section identity)

(defn change-section
  ([section]
   (change-section section true))
  ([section save-history?]
   (if save-history? (.pushState (.-history js/window) (name section) (name section)))
   (validate-section section)
   (reset! app-state
       (loop [sec (seq @app-state) past true cum {}]
         (let [s (first sec)
               k (get s 0)
               v (get s 1)
               current (= section k)
               past (if current false past)]
           (if (pos? (count sec))
             (recur (rest sec) past
                    (assoc cum k (assoc v :current current :past past)))
             cum))))))

(defn section-indicators [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (html
        [:ul
         (for [[k v] (seq data)]
            (if (:valid v)
              [:li (merge {:class [(if (:past v) "past")
                            (if (:current v) "current")]
                           :key k
                           }
                    (fast-click #(if (:valid v) (change-section k))))
               [:div {:class "circle-container"}
                [:a (copy (keyword (str (name k) "-tooltip")))]]]))]))))


(defn hook-browser-navigation! [section]
  (.pushState (.-history js/window) (name section) (name section))
  (events/listen js/window
   EventType/POPSTATE
   (fn [event]
     (change-section (keyword (.-state event)) false))))

