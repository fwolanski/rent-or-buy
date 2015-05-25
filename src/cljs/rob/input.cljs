(ns rob.input
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]])
  (:use [rob.copy :only [format-currency-input format-percent format-dollars format-years]]
        [rob.events :only [fast-click]]
        ))


(defn clean [s]
  (let [v  (js/parseFloat (clojure.string/replace (str s) #"[^0-9.]" ""))]
    (when-not (js/isNaN v) v)))

(defn between [data value]
  (if (and (contains? data :min) (contains? data :max))
    (<= (clean value) (:max data))
    true))

(defn validate-regex [e owner text data regex]
  (let [value (.. e -target -value)]
    (if (and (re-find regex value) (between data value))
      (do
        (om/set-state! owner :text value)
        (let [v (clean value)]
          (if-not (= (:value data) v)
            (om/update! data :value v))))
      (om/set-state! owner :text text))))


(defmulti format :format )

(defmethod format :currency [data]
  (format-currency-input (:value data)))

(defmethod format :dollars [data]
  (format-dollars (:value data)))

(defmethod format :percent [data]
  (format-percent (:value data) (:accuracy data)))

(defmethod format :years [data]
  (format-years (:value data)))


(defmulti validate (fn [e owner {:keys [text]} data] (:format data)))

(defmethod validate :currency [e owner {:keys [text]} data]
  (validate-regex e owner text data #"^(\$)?[0-9, ]*(\.[0-9]{0,2})?$"))

(defmethod validate :dollars [e owner {:keys [text]} data]
  (validate-regex e owner text data #"^(\$)?[0-9, ]*$"))

(defmethod validate :percent [e owner {:keys [text]} data]
  (validate-regex e owner text data #"^0*(100|[0-9]?[0-9]?)(\.[0-9]*)?(\%)?$"))

(defmethod validate :years [e owner {:keys [text]} data]
  (validate-regex e owner text data #"^0*([0-9]?[0-9]?)[ a-zA-Z]*$"))


(defn hide-on-enter [e]
  (let [k (.-keyCode e)]
    (if (= k 13) (.blur (.-target e)))))

(defn input [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text (if (:value data) (format data) "")
       :active false})
    om/IWillReceiveProps
    (will-receive-props [_ props]
      (let  [inactive (not (om/get-state owner :active))
             has-value (:value data)]
        (if (and inactive has-value) (om/set-state! owner :text (format props)))))
    om/IRenderState
    (render-state [_ state]
      (html
        [:div {:class ["input" (if (:disabled data) "disabled")]}
          [:div {:class "input-top"}]
          [:input (merge
                    {:type "text" :required true
                     :value (:text state)
                     :onChange #(validate % owner state data)
                     :onFocus #(om/set-state! owner :active true)
                     :onKeyDown hide-on-enter
                     :onBlur #(do
                                (om/set-state! owner :active false)
                                (om/set-state! owner :text (if (:value data) (format data) "")))}
                    (if (:disabled data) {:disabled true}))]
         [:span {:class "bar"}]
         [:label (:name state)]]))))
