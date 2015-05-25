(ns rob.state)

(def app-state
  (atom {

    :welcome
    {:current true
     :valid true
     }

    :people
    {:current false
     :valid false
     }

    :income
    {:current false
     :valid false

     }

    :savings
    {:current false
     :valid false
     }

    :compare
    {:current false
     :valid false
     :assumptions
     {:inflation
      {:value 2 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 2}
      :rate-of-return
      {:value 6 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 6}
      :income-growth
      {:value 4 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 4}
      :rent-growth
      {:value 1 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 1}
      :home-growth
      {:value 2 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 2}
      :mortgage-interest
      {:value 3 :format :percent :min 0 :max 20 :ticks 5 :accuracy 2 :default 3}
      :amortization
      {:value 25 :format :years :min 1 :max 30 :ticks 7 :default 25}
      :closing-costs
      {:value 1 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 1}
      :selling-closing-costs
      {:value 4 :format :percent :min 0 :max 10 :ticks 5 :accuracy 2 :default 4}
      :maintainance-fees
      {:value 1 :format :percent :min 0 :max 20 :ticks 5 :accuracy 2 :default 1}
      :property-tax
      {:value 1 :format :percent :min 0 :max 20 :ticks 5 :accuracy 2 :default 1}
      :years
      {:value 26} } }
 }))

(defn value-from-format [item]
  (let [v (:value item)
        v (if (nil? v) (:default item) v) ]
    (case (:format item)
      :percent (/ v 100)
      v)))

(defn assume [k]
  (case k
    :income (get-in @app-state [:income :total-income])
    :monthly-gross (get-in @app-state [:income :monthly-gross])
    :incomes (get-in @app-state [:income :incomes])
    :people (get-in @app-state [:people :people])
    :spending (get-in @app-state [:savings :total-spend])
    :savings (get-in @app-state [:savings :total-saved])
    :savings-after-spend (get-in @app-state [:savings :total-saved-after-spend])
    :savings-percent (get-in @app-state [:savings :save-percent])
    :savings-period (get-in @app-state [:savings :save-period])
   (value-from-format (get-in @app-state [:compare :assumptions k]))))

(defmulti watch-section (fn [section state old-state] section))

(defmethod watch-section :default [section _ _] nil)

(defn watch-fn [_ _ os ns]
  (doseq [k (keys ns)]
    (if (not= (k ns) (k os))
      (let [n (watch-section k (k ns) (k os) )]
        (if-not (nil? n)
          (swap! app-state assoc k n))))))

(add-watch app-state nil watch-fn)


(defn has-changed? [n o ks]
  (not= (get-in o ks) (get-in n ks)))

(defn bind-states [n o bindings]
  (let [bind (map (fn [[k v]]
                    (if (has-changed? n o k) (v n)))
                  bindings)
        non-empty (remove nil? bind)
        merged (apply merge non-empty)
        m  (reduce (fn [m [k v]] (assoc-in m k v)) n merged)]
    (if (empty? non-empty) n m)))
