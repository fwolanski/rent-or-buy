(ns rob.sections.income
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:use
    [rob.events :only [fast-click]]
    [rob.state :only [watch-section]]
    [rob.copy :only [copy keywords-to-copy format-currency format-dollars]]
    [rob.input :only [input]]
    [rob.dropdown :only [dropdown]]
    [rob.math :only [federal-taxes quebec-taxes]]
    [rob.section :only [change-section validate-section get-section set-section! make-from-cardinalty]]))

(defn income-relations [income timeframe]
  (let [income-calcs {
   :yearly { :biweekly (fn [x] (/ x 26)) :monthly (fn [x] (/ x 12)) }
   :biweekly { :yearly (fn [x] (* x 26)) :monthly (fn [x] (/ (* x 26) 12)) }
   :monthly { :biweekly (fn [x] (/ (* x 12) 26)) :yearly (fn [x] (* x 12)) }}]
    (merge {timeframe income}
           (apply merge (map (fn [[k v]] {k (v income)} ) (timeframe income-calcs))))))

(defn income-to-taxes [income-map taxes]
  (let [fed (if (= taxes :after-tax) 0 (federal-taxes (:yearly income-map)))
        que (if (= taxes :after-tax) 0 (quebec-taxes (:yearly income-map)))
        ati (- (:yearly income-map) fed que)]
    (merge income-map
      {
       :federal-yearly fed
       :federal-monthly (/ fed 12)
       :federal-biweekly (/ fed 26)

       :provincial-yearly que
       :provincial-monthly (/ que 12)
       :provincial-biweekly (/ que 26)

       :after-tax-yearly ati
       :after-tax-monthly (/ ati 12)
       :after-tax-biweekly (/ ati 26)})))

(defn financial-table [props]
  (let [income (-> props :income :value)
        date (-> props :dates :value)
        taxes (-> props :taxes :value)
        income-map (income-relations income date)]
    (income-to-taxes income-map taxes)))

;; todo: this is rough, fix this
(defn estimate-gross [monthly]
  (* 1.3 monthly ))

(defn can-proceed? [data]
  (let [two (= 2 (get-section :people :people))
        proceed (not (nil? (get-in data [:input 0 :income :value])))]
    (if two (and proceed (not (nil? (get-in data [:input 1 :income :value])))) proceed)))

(defn has-income? [data]
  (not (or (nil? data) (zero? data))))

(defn sum-income [data]
  (reduce + (map #(-> (financial-table %) :after-tax-yearly) (:input data))))

(defn sum-gross [data]
  (estimate-gross (reduce + (map #(-> (financial-table %) :after-tax-monthly) (:input data)))))


(defn move-to-savings [] (change-section :savings))

(defn make-data [current people]
  (make-from-cardinalty current people
    {:income {:format :currency},
     :taxes {:value :before-tax},
     :dates {:value :yearly} }))

(defmethod validate-section :income [section]
  (let [people (get-section :people :people)
        current (get-section :income)
        data (make-data (:input current) people)]
    (set-section! :income
      (assoc current :valid true :input data))
    (if-not (can-proceed? current)
      (do
        (set-section! :savings :valid false)
        (set-section! :compare :valid false)))))

(defmethod watch-section :income [_ state _]
  (if (can-proceed? state)
    (merge state {:total-income (sum-income state)
                  :monthly-gross (sum-gross state)
                  :incomes (map #(-> (financial-table %) :after-tax-yearly)
                                (:input state))})))

(defn income-table [data owner]
  (reify
    om/IInitState
    (init-state [_]
      (financial-table data))
    om/IWillReceiveProps
    (will-receive-props [_ props]
      (om/update-state! owner #(financial-table props)))
    om/IRenderState
    (render-state [_ state]
      (letfn [(row [copy-key currency-key1 currency-key2]
                [:tr
                 [:td {:class "header"}
                  [:span {:class "income-header"} (copy copy-key)]
                  [:span {:class "income-header-mobile"}
                   (copy (keyword (str (name copy-key) "-mobile")))]]
                 [:td {:class "money"} (format-currency (currency-key1 state) false) ]
                 [:td {:class "money"} (format-currency (currency-key2 state) false)]])]
        (let [has-taxes? (not= (-> data :taxes :value) :after-tax)]
          (html
          [:div {:class ["table-wrapper" (if (has-income? (-> data :income :value)) "active")]}
           [:table {:class "table" }
            [:tbody
             [:tr {:class "header"}
              [:td ]
              [:td (copy :yearly-header)]
              [:td (copy :biweekly-header)]
              ]

             (if has-taxes? (row :income-top :yearly :biweekly))
             (if has-taxes? (row :provincial-tax :provincial-yearly :provincial-biweekly))
             (if has-taxes? (row :federal-tax :federal-yearly :federal-biweekly))
             (row :net-income :after-tax-yearly :after-tax-biweekly )

            ]]]))))))


(defn income-input [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (html
        [:div {:class "group"}
         [:div {:class "income-input"}
          [:div {:class "income-input-item"}
           (om/build input (:income data)
                     {:init-state {:name
                                   (if
                                     (= (:place state) 0)
                                     (copy :my-income)
                                     (copy :partners-income))}})]
          [:div  {:class "income-input-item"}
           (om/build dropdown (:dates data)
                     {:init-state {:values (keywords-to-copy [:yearly :monthly :biweekly ])}})]
          [:div  {:class "income-input-item"}
           (om/build dropdown (:taxes data)
                     {:init-state {:values (keywords-to-copy [:before-tax :after-tax])}})]]
          (om/build income-table data)]))))


(defn income [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [card (get-section :people :people)]
        (html
         [:section {:class ["income"
                            (if (:current data) "active")
                            (if (= card 2) "two-incomes")]}
         [:div {:class ["subsection"] }
          [:h2 (if (= card 1) (copy :income-title-alone) (copy :income-title-together))]
          [:div {:class "description"} (copy :income-desc)]]
         [:div {:class ["subsection"]}
          (map-indexed #(om/build income-input %2 {:init-state {:place %1} :react-key %1}) (data :input))
          [:div {:class ["income-button-container"] }
           (if (can-proceed? data)
             [:div (merge
                     {:class ["button" "income-button"]}
                     (fast-click move-to-savings))
              (let [txt (if (= card 2) :our-total-income :my-total-income)]
                 (str (copy txt) " "
                      (format-dollars (:total-income data) false) " "
                      (copy :per-year-after-tax)))])]]])))))

