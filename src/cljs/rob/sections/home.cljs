(ns rob.sections.home
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:use [rob.events :only [fast-click]]
        [rob.section :only [change-section validate-section set-section!]]
        [rob.copy :only [copy copy-p]]))


(defn welcome [] (change-section :people))

(defmethod validate-section :welcome [section]
  (set-section! :welcome :valid true))

(defn home [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (html
        [:section {:class ["home" (if (:current data) "active")]}
         [:div {:class ["subsection" ] }
          [:div {:class "home-container"}
           [:h2  (copy :welcome-title)]
           [:div {:class "description"}  (copy-p :welcome-desc)]
           [:div {:class "center-button"}
            [:div (merge {:class "button"} (fast-click welcome)) (copy :welcome-button) ]]
          ]]]))))
