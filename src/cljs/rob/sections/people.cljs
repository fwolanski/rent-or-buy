(ns rob.sections.people
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:use
    [rob.events :only [fast-click]]
    [rob.copy :only [copy]]
    [rob.section :only [change-section validate-section set-section!]]))


(defmethod validate-section :people [section]
  (set-section! :people :valid true))

(defn select-number [data selection]
  (do
    (om/update! data [:people] selection)
    (change-section :income)))

(defn button-class [data people]
  (if (not (nil? (-> data :people)))
    (if (= (-> data :people) people) "selected" "not-selected")))

(defn people [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (html
        [:section {:class ["people" (if (:current data) "active")]}
         [:div {:class ["subsection"] }
          [:h2  (copy :people-title)]
          [:div {:class "description"}  (copy :people-desc)]
          ]
         [:div {:class ["subsection"]}
          [:div {:class "choice"}
           [:div {:class "option"}
            [:div (merge {:class ["button" (button-class data 1)]}
                   (fast-click #(select-number data 1))) (copy :people-one)]]
           [:div {:class "option"}
            [:div (merge {:class ["button" (button-class data 2)]}
                   (fast-click #(select-number data 2))) (copy :people-two) ]]]]]))))

