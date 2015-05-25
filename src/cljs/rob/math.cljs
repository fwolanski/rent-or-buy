(ns rob.math
  (:use
    [rob.state :only [assume]]))


(def federal-rates [[44701 0.15] [44700 0.22] [49185 0.26] [0 0.29]])
(def quebec-rates [[41935 0.16] [41930 0.20] [18175 0.24] [0 0.2575]])

(def federal-personal 11138)
(def quebec-personal 11305)

(def mortgage-default-insurance [[0.10 0.0315] [0.15 0.024] [0.2 0.018] [1 0]])

(def welcome-tax [[50000 0.005] [200000 0.01] [250000 0.015] [0 0.02]])

(def ideal-from-gross 0.2)

(defn non-ref-tax-cred [rates taxes income]
  (let [red (/ taxes income)
        cred (- taxes (* red rates))]
    (if (pos? cred) cred 0)))

(defn income-tax [rates income]
   (let [[amt-adj rate] (first rates)
         amt (if (zero? amt-adj) income amt-adj)
         remaining (- income amt)
         taxable (if (pos? remaining) amt income)
         taxes (* taxable rate)]
     (if (pos? remaining)
       (+ taxes (income-tax (rest rates) remaining))
        taxes)))

(defn taxes [rates non-ref income]
  (let [tax (income-tax rates income)]
    (non-ref-tax-cred non-ref tax income)))

(def federal-taxes (partial taxes federal-rates federal-personal))
(def quebec-taxes (partial taxes quebec-rates quebec-personal))
(def land-transfer (partial income-tax welcome-tax))

(defn mortgage-insurance [rates price down-payment]
   (let [dtp (/  down-payment price)
         loan (- price down-payment)
         rate (first (filter (fn [[r p]] (< dtp r)) rates))]
     (* loan (get rate 1))))


(defn max-rent []
  (/ (assume :income) 12))

(defn ideal-rent []
  (* ideal-from-gross (assume :monthly-gross)))


(defn principle [price]
  (let [l (- price (assume :spending))
        l (+ l (mortgage-insurance mortgage-default-insurance price (assume :spending)))]
   (if (< l 0) 0 l)))

(defn monthly-mortgage-payment [price]
  (let [l (principle price)
        i (/ (assume :mortgage-interest) 12)
        n (* (assume :amortization) 12)]
   (/ (* l i) (- 1 (/ 1 (.pow js/Math (+ 1 i) n))))))

(defn loan-from-payment [payment]
  (let [i (/ (assume :mortgage-interest) 12)
        n (* (assume :amortization) 12)]
   (/ (- payment (* (.pow js/Math (+ 1 i) (- n) ) payment)) i)))


(defn maintainance [value]
  (* value (assume :maintainance-fees)))

(defn property-tax [value]
  (* value (assume :property-tax)))


(defn closing-costs [price]
  (* (assume :closing-costs) price))

(defn available-down-payment [price]
  (- (assume :spending)
     (land-transfer price)
     (closing-costs price)))


(defn ideal-home-price []
  (let [loan (loan-from-payment (* ideal-from-gross (assume :monthly-gross)))
        dp (available-down-payment loan)
        dp-limit (/ (assume :spending) (+ 0.05 0.02 (assume :closing-costs) )) ]
    (min (+ loan dp) dp-limit)))

(defn max-home-price []
  (min
    (/ (assume :spending) (+ 0.05 0.02 (assume :closing-costs) ))
    (let [loan (loan-from-payment (* 0.32 (assume :monthly-gross)))]
      (+ loan (available-down-payment loan)))))


(defn house-down-payment [price]
  (min (available-down-payment price) price))

(defn down-payment [price]
  (+ (min (available-down-payment price) price)
     (land-transfer price)
     (closing-costs price)))

(defn monthly-payment [price]
  (+ (monthly-mortgage-payment price)
     (/ (maintainance price) 12)))

(defn amount-owed-after [principle year monthly-payment]
  (let [interest (/ (assume :mortgage-interest) 12)
        acc  (.pow js/Math (+ 1 interest) (* 12 year))
        left (- (* principle acc) (* (/ (- acc 1) interest) monthly-payment))]
    (if (neg? left) 0 left)))

(defn interest-on [p i]
  (* p i))

(defn compound [p i q n]
  (* p (.pow js/Math (inc (/ i q)) (* n q) )))

(defn future-value [p i q n]
 (* p (* (- (.pow js/Math (inc (/ i q)) (* n q)) 1) (/ q i))))




(defn savings [principle principle-alt save year comp-period save-period]
  (let [contrib (compound save (assume :income-growth) save-period year)
        total (+ (compound principle (assume :rate-of-return) comp-period 1)
                 (future-value contrib (assume :rate-of-return) save-period 1))
        total-m (+ (compound principle-alt (assume :rate-of-return) comp-period 1)
                   (future-value contrib (assume :rate-of-return) save-period 1))]
    {:total total
     :deinflated (compound total (- (assume :inflation)) 1 year)
     :with-m-total total-m
     :with-m-deinflated (compound total-m (- (assume :inflation)) 1 year)}))

(defn calc-savings [principle keepings save save-period comp-period]
  (loop [years (range 30) t {}]
    (if (empty? years) t
      (let [year (first years)]
        (recur
          (rest years)
          (assoc t (inc (first years))
                 (savings
                   (if (empty? t) principle (:total (get t year)))
                   (if (empty? t) keepings (:with-m-total (get t year)))
                   save year comp-period save-period)))))))

(defn calc-savings-with-assumptions []
  (map #(calc-savings
          (nth (assume :savings) %)
          (nth (assume :savings-after-spend) %)
          (* (/ (nth (assume :incomes) %)
                (nth (assume :savings-period) %))
             (nth (assume :savings-percent) %))
          (nth (assume :savings-period) %)
          1)
       (range (assume :people))))

(defn sum-savings [savings]
  (reduce
    (fn [m v]
      (apply merge
        (map (fn [[k1 v1]]
               {k1
                {:total  (+ (:total v1) (get-in m [k1 :total] 0))
                 :with-m-total  (+ (:with-m-total v1) (get-in m [k1 :with-m-total] 0))}})
             v)))
    {} savings))

(defn calc-assets [price rent]
  (let [monthly-mortgage-payment (monthly-mortgage-payment price)
        principle (principle price)
        down-payment (down-payment price)]
   (loop [years (range (assume :years)) t {}]
    (if (empty? years) t
      (let [year (first years)
            home-worth (compound price (assume :home-growth) 1 year)
            rent-adj (* 12 (compound rent (assume :rent-growth) 1 year))
            always-pay (+ (maintainance home-worth) (property-tax home-worth))
            monthly-payment (if (< (assume :amortization) year)
                              always-pay
                              (+ (* 12 monthly-mortgage-payment) always-pay))
            total-monthly (if (empty? t) monthly-payment
                            (+ monthly-payment (-> (get t (dec year)) :buying :recurring)))
            total-rent-monthly (if (empty? t) rent-adj
                         (+ rent-adj (-> (get t (dec year)) :renting :recurring)))
            opportunity-buy (if (empty? t)
                              (interest-on (+ down-payment monthly-payment)
                                           (assume :rate-of-return))
                              (+ (interest-on monthly-payment (assume :rate-of-return))
                                 (compound (-> (get t (dec year)) :buying :opportunity)
                                           (assume :rate-of-return) 1 1)))
            opportunity-rent (if (empty? t)
                              (interest-on rent-adj (assume :rate-of-return))
                              (+ (interest-on rent-adj (assume :rate-of-return))
                                 (compound (-> (get t (dec year)) :renting :opportunity)
                                           (assume :rate-of-return) 1 1)))
            mortgage (amount-owed-after principle year monthly-mortgage-payment)
            closing (* (assume :selling-closing-costs) home-worth)
            profits (- (+ mortgage closing)  home-worth)
            total-buy (+ down-payment total-monthly opportunity-buy profits)
            total-rent (+ total-rent-monthly opportunity-rent)
            yearly-avg-buy (/ total-buy (inc year))
            yearly-avg-rent (/ total-rent (inc year))
            monthly-avg-buy (/ yearly-avg-buy 12)
            monthly-avg-rent (/ yearly-avg-rent 12) ]
        (recur
          (rest years)
          (assoc t year
             {:buying
              {:initial down-payment
               :recurring total-monthly
               :opportunity opportunity-buy
               :profits profits
               :total total-buy
               :year-average yearly-avg-buy
               :monthly-average monthly-avg-buy
               :mortgage mortgage
               :home-worth home-worth
               }
              :renting
              {:initial 0
               :recurring total-rent-monthly
               :opportunity opportunity-rent
               :profits 0
               :total total-rent
               :year-average yearly-avg-rent
               :monthly-average monthly-avg-rent
               }})))))))



