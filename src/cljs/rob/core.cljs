(ns rob.core
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [goog.dom :as gdom]
            [goog.style :as gstyle]
            [goog.events :as events]
            [goog.events.MouseWheelHandler]
            [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]])
(:use
    [rob.events :only [fast-click]]
    [rob.state :only [app-state]]
    [rob.copy :only [copy]]
    [rob.section :only [section-indicators section-name set-section! validate-section change-section  hook-browser-navigation!]]
    [rob.sections.home :only [home]]
    [rob.sections.people :only [people]]
    [rob.sections.income :only [income]]
    [rob.sections.compare :only [compare-comp]]
    [rob.sections.savings :only [savings]]))

(enable-console-print!)
(hook-browser-navigation! :welcome)

(defn root-view [data]
  (reify
    om/IRender
    (render [_]
      (letfn [(is-active? [k] (if (get-in data [k :current]) "active" ))
              (is-past? [k] (if (get-in data [k :past]) "past" ))
              (get-active [] (ffirst (filter (fn [[k v]] (:current v)) data)))
              (make-section [k f]
                [:div {:class ["section-wrapper" (is-active? k) (is-past? k)]}
                 (om/build f (get data k) {:react-key k})]) ]
        (let [previous-section (first (last (filter (fn [[k v]] (:past v)) data)))
              next-section (ffirst
                             (filter (fn [[k v]]
                                       (and (:valid v) (not (:current v)) (not (:past v)))) data))]
          (html
            [:div {:class "container"}
             [:header
              [:a {:class ["logo" (if previous-section "mobile-hide")]
                   :href "//filipwolanski.com"}]
              (if previous-section
                [:div (merge {:class "mobile-back-navigation"}
                             (fast-click #(change-section previous-section))) ])
              [:div {:class "mobile-title"} (copy (keyword (str (name (get-active)) "-mobile")) )]
              (if next-section
                [:div (merge {:class "mobile-forward-navigation"} (fast-click #(change-section next-section))) ])
              [:div {:class ["nav"] } (om/build section-indicators data)]
              ]
             [:article
              [:div {:class ["section-bk" (str "section-" (section-name))]}]
              (make-section :welcome home)
              (make-section :people people)
              (make-section :income income)
              (make-section :savings savings)
              (make-section :compare compare-comp)
              ]]))))))

(om/root root-view app-state {:target (gdom/getElement "root")})

; just enough time to get everyting into place
(.setTimeout js/window #(gstyle/setStyle
                          (gdom/getElement "intro-spinner") "display" "none") 400)

; (change-section :people)
; (set-section! :people :people 2)

; (change-section :income)

; (set-section! :income :input 0 :income :value 50000)
; (set-section! :income :input 1 :income :value 65000)

; (change-section :savings)

; (set-section! :savings :input 0 :savings :value 50000)
; (set-section! :savings :input 0 :save-percent :value 10)

; (set-section! :savings :input 1 :savings :value 20000)
; (set-section! :savings :input 1 :save-percent :value 15)

; (change-section :compare)

