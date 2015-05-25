(ns rob.events
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <! mult sliding-buffer]]
            [goog.events :as events]
            [goog.events.EventType :as EventType]))


(defn fast-click [f]
  {:onClick f
   :onTouchStart (fn [e]
                   (.preventDefault e)
                   (.stopPropagation e)
                   (f))})


(def position-chan (chan (sliding-buffer 1)))
(def up-chan (chan (sliding-buffer 1)))
(def down-chan (chan (sliding-buffer 1)))

(def movement (mult position-chan))
(def up (mult up-chan))
(def down (mult down-chan))


(defn e-position [e] (.-clientX e))

(defn touch-move
  [e]
  (e-position (aget (.-changedTouches e) 0)))

(defn track-movement
  [e extract-pos]
  (put! position-chan (extract-pos e)))

(defn up-fn [e]
  (do
    (put! position-chan :end)
    (put! up-chan :up)))

(defn down-fn [e]
  (do
    (put! down-chan :down)))

(.addEventListener js/window "mousemove" #(track-movement % e-position))
(.addEventListener js/window "touchmove" #(track-movement % touch-move))

(.addEventListener js/window "mouseup" #(up-fn %))
(.addEventListener js/window "touchend" #(up-fn %))
(.addEventListener js/window "touchcancel" #(up-fn %))


(.addEventListener js/window "mousedown" #(down-fn %))
(.addEventListener js/window "touchstart" #(down-fn %))
