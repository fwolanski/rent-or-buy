(ns rob.sections.savings
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [om.core :as om :include-macros true]
            [rob.math :as math]
            [sablono.core :as html :refer-macros [html]])
  (:use
    [rob.events :only [fast-click]]
    [rob.state :only [watch-section bind-states assume]]
    [rob.copy :only [copy keywords-to-copy format-percent format-dollars]]
    [rob.dropdown :only [dropdown]]
    [rob.input :only [input]]
    [rob.slider :only [slider]]
    [rob.section :only [change-section validate-section get-section set-section!]]))



(defn make-from-cardinalty [k people form]
  (let [i (if (empty? k) [] k)
        missing (- people (count i))]
    (cond
      (neg? missing) (vec (drop-last (- missing) i))
      (pos? missing) (apply conj i (repeat missing form))
      :else i)))

(defn make-data [current people]
  (make-from-cardinalty current people
    {:savings {:format :currency }
     :spend-percent {:format :percent, :min 0, :max 100,
                     :value 100, :ticks 0, :accuracy 0,
                     :disabled true, :no-move true}
     :spend-currency {:format :currency, :min 0, :max 100,
                      :disabled true}
     :dates {:value :yearly}
     :save-currency {:format :currency, :min 0}
     :save-percent {:format :percent, :min 0, :max 100,
                    :ticks 0, :accuracy 0,
                    :no-move true}
     }))

(defn percent [v p] (* v (/ p 100)))

(defn set-max-save [data incomes]
  (reduce (fn [m [i v]]
            (assoc-in m [i :save-currency]
                      (merge (get-in m [i :save-currency])
                             {:max v})))
          data incomes))

(defmethod validate-section :savings [section]
  (let [people (get-section :people :people)
        incomes (map-indexed vector (assume :incomes))
        current (get-section :savings)
        data (make-data (:input current) people)
        data (set-max-save data incomes)]
    (set-section! :savings
      (assoc current :valid true :input data))))


(defn has-savings? [data]
  (and
    (not (nil? (get-in data [:savings :value])))
    ; (not (nil? (get-in data [:save-currency :value])) )
    ))

(defn can-proceed? [data]
  (let [two (= 2 (get-section :people :people))
        proceed (has-savings? (get-in data [:input 0]))]
    (if two (and proceed (has-savings? (get-in data [:input 1]))) proceed)))


(defn input-bindings [idx]
  {[:savings :value]
    (fn [state]
      (let [v (-> state :savings :value)]
        {[:spend-currency :value]
         (* (-> state :savings :value)
            (/ (-> state :spend-percent :value) 100))

         [:spend-currency :max]
         (-> state :savings :value)

         [:spend-currency :disabled]
         (or (zero? v) (nil? v))

         [:spend-percent :disabled]
         (or (zero? v) (nil? v))

         }))

   [:spend-percent :value]
    (fn [state]
      {[:spend-currency :value]
       (if (-> state :spend-currency :disabled) 0
        (percent (-> state :savings :value) (-> state :spend-percent :value)))})

   [:spend-currency :value]
   (fn [state]
     (if (-> state :savings :value)
       {[:spend-percent :value]
        (if (-> state :spend-percent :disabled) 100
          (* 100
           (/ (-> state :spend-currency :value)
              (-> state :savings :value))))}))

    [:dates :value]
    (fn [state]
      (let [income (nth (assume :incomes) idx)
            income-v {:yearly income
                      :monthly (/ income 12)
                      :biweekly  (/ income 26)}
            has-save-percent? (not (nil? (-> state :save-percent :value)))]
        (merge {[:save-currency :max]
                ((-> state :dates :value) income-v)}
               (if has-save-percent? {[:save-currency :value]
                                      (percent
                                        ((-> state :dates :value) income-v)
                                        (-> state :save-percent :value))}))))

    [:save-percent :value]
    (fn [state]
      (let [income (nth (assume :incomes) idx)
            income-v {:yearly income
                      :monthly (/ income 12)
                      :biweekly  (/ income 26)}]
        {[:save-currency :value]
         (percent ((-> state :dates :value) income-v)
                  (-> state :save-percent :value))}))


    [:save-currency :value]
    (fn [state]

      (let [income (nth (assume :incomes) idx)
            income-v {:yearly income
                      :monthly (/ income 12)
                      :biweekly  (/ income 26)}
            no-income? (zero? (:yearly income-v))]
        {[:save-percent :value]
         (if no-income? 0
           (* 100
            (/ (-> state :save-currency :value)
               ((-> state :dates :value) income-v))))}))

   })

(defn merge-input-bindings [state old-state]
  (vec
    (map-indexed
      (fn [i x] (bind-states
                (get-in state [:input x])
                (get-in old-state [:input x])
                (input-bindings i)))
      (range (get-section :people :people)))))


(defn calc-total [state]
  (reduce (fn [m v] (+ m (-> v :spend-currency :value))) 0 (:input state)))

(defn get-value [k v]
  (get-in v [k :value]))

(defn period-to-val [data]
  (case (get-in data [:dates :value])
    :yearly 1
    :monthly 12
    :biweekly 26))

(defmethod watch-section :savings [_ state old-state]
  (if (:valid state)
    (let [merged (merge-input-bindings state old-state)
          total (calc-total state)
          total-saved  (map (partial get-value :savings) (-> state :input))
          total-saved-after  (map - total-saved
                                  (map (partial get-value :spend-currency) (-> state :input)))
          save-percent (map #(/ % 100)
                            (map (partial get-value :save-percent) (-> state :input)))
          save-period (map period-to-val (-> state :input))]
      (if (not (nil? merged))
        (assoc state
               :input merged
               :total-spend total
               :total-saved total-saved
               :total-saved-after-spend total-saved-after
               :save-percent save-percent
               :save-period save-period)))))

(defn move-to-compare [] (change-section :compare))


(defn calc-savings-table [data]
  (let [savings (get-in data [:savings :value])
        keepings (- savings (get-in data [:spend-currency :value]))
        save (get-in data [:save-currency :value])
        period (period-to-val data)]
    (math/calc-savings savings keepings save period 1)))

(defn savings-table [data owner]
  (reify
    om/IInitState
    (init-state [_]
      (calc-savings-table data))
    om/IWillReceiveProps
    (will-receive-props [_ props]
      (om/update-state! owner #(calc-savings-table props)))
    om/IRenderState
    (render-state [_ state]
      (letfn [(row [year savings deinflated with-m-savings with-m-deinflated]
                [:tr
                 [:td {:class "header year"} [:span {:class "year"} (str year)]]
                 [:td {:class "money"} (format-dollars savings false) ]
                 [:td {:class "money total"} (format-dollars deinflated false)]
                 [:td {:class "money"} (format-dollars with-m-savings false) ]
                 [:td {:class "money total"} (format-dollars with-m-deinflated false)]])]
        (html
          [:div {:class ["table-wrapper" (if (has-savings? data) "active")]}
           [:table {:class "table" }
            [:tbody
             [:tr {:class "header"}
              [:td {:class "year" :rowSpan "2"} (copy :year)]
              [:td {:colSpan "2"} (copy :without-m)]
              [:td {:colSpan "2"} (copy :with-m)]
              ]
             [:tr {:class "header"}
              [:td (copy :saved-total)]
              [:td (copy :saved-deinflated)]
              [:td (copy :saved-total)]
              [:td (copy :saved-deinflated)]
              ]
             (for [year [5 10 15 25]]
              (row
                year
                (:total (get state year))
                (:deinflated (get state year))
                (:with-m-total (get state year))
                (:with-m-deinflated (get state year))
                ))
             ]]
            [:div {:class "table-disclaimer"}
             (copy :savings-disclaimer)]])))))


(defn savings-input [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (letfn [(input-item [data-key copy-my copy-partner]
                [:div {:class "savings-input-item"}
                 (om/build input (data-key data)
                           {:init-state {:name
                                         (if
                                           (= (:place state) 0)
                                           (copy copy-my)
                                           (copy copy-partner))}})])]
        (html
          [:div {:class "group"}
           [:div {:class "savings-input"}
            (input-item :savings :my-savings :partners-savings)
            ; (input-item :spend-currency :my-spend :partners-spend)
            ; (input-item :spend-percent :my-spend-percent :partners-spend-percent)

            [:div {:class "savings-input-item"}
             [:div {:class "text"} (if
                                     (= (get-section :people :people) 2)
                                     (copy :our-home)
                                     (copy :my-home))]]

            ; (input-item :save-currency :my-save :partners-save)

            ; [:div {:class "savings-input-item with-border"}
            ;  (om/build dropdown (:dates data)
            ;            {:init-state
            ;             {:values (keywords-to-copy [:yearly :monthly :biweekly ])}})]

            ; (input-item :save-percent :my-save-percent :partners-save-percent)

            ; [:div {:class "savings-input-item"}
            ;  [:div {:class "text"} (if
            ;                          (= (:place state) 0)
            ;                          (copy :of-my-income)
            ;                          (copy :of-partners-income))]]

            ]
           ; (om/build savings-table data)

           ])))))


(defn savings [data owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (let [card (get-section :people :people)]
        (html
         [:section {:class ["savings"
                            (if (:current data) "active")
                            (if (= card 2) "two-incomes")]}
         [:div {:class ["subsection"] }
          [:h2 (if (= card 1) (copy :savings-title-alone) (copy :savings-title-together))]
          [:div {:class "description"} (copy :savings-desc)]]
         [:div {:class ["subsection"]}
          (map-indexed #(om/build savings-input %2 {:init-state {:place %1}}) (data :input))
          [:div {:class ["savings-button-container"] }
           (if (can-proceed? data)
             [:div (merge {:class ["button" "savings-button"] } (fast-click move-to-compare))
              (let [txt (if (= card 2) :we-can-spend :i-can-spend)
                    end (if (= card 2) :towards-our-home :towards-my-home)]
                 (str (copy txt) " "
                      (format-dollars (:total-spend data) false) " "
                      (copy end)))])]]])))))

