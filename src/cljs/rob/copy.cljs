(ns rob.copy
  (:require
    [goog.string :as gstring]
    [goog.string.format]
    [clojure.string :as string]
    )
  (:use
    [rob.state :only [assume]]))


(def year (.getFullYear (js/Date.)))



(defn round-to-2 [v]
  (let [rounded (.round js/Math (* 100 v))]
    (gstring/format "%.2f" (/ rounded 100))))


(defn round-to-n [v n]
  (let [rounded (.round js/Math (* (.pow js/Math 10 n) v))]
    (gstring/format (str "%." n "f") (/ rounded (.pow js/Math 10 n)))))

(defn round-to-0 [v]
  (str (.round js/Math v)))

(defn round-to-2-or-0 [v]
  (let [rounded (.round js/Math (* 100 v))]
    (if (zero? (mod rounded 100))
      (str (/ rounded 100))
      (gstring/format "%.2f" (/ rounded 100)))))

(defn commify [s rounding-fn]
   (string/replace (rounding-fn s) #"\B(?=(\d{3})+(?!\d))" "," ))

(defn max-3 [v]
  (let [string (gstring/format "%.2f" v)]
    (if (= 6 (.-length string))
      (.substring string 0 3)
      (.substring string 0 4))))

(defn change-to-3 [o]
  (let [append ["" "K" "M" "B" "T"]]
    (loop [v o a append]
      (if (< (/ v 1000) 1)
       (str (max-3 v) (first a))
       (recur (/ v 1000) (rest a))))))


(defn format-percent
  ([value] (format-percent value 0))
  ([value digit] (str (round-to-n value digit) "%")))


(defn format-currency
  ([value] (format-currency value true))
  ([value space] (str (if space "$ " "$") (commify value round-to-2))))

(defn format-dollars
  ([value] (format-dollars value true))
  ([value space] (str (if space "$ " "$") (commify value round-to-0))))



(defn format-currency-3
  ([value] (format-currency-3 value true))
  ([value space] (str (if space "$ " "$") (change-to-3 value))))


(defn format-currency-input [value]
  (if (nil? value)
    ""
    (str "$ " (commify value round-to-2-or-0))))

(def nbsp (.fromCharCode js/String 160))



(def copy-english {

  :welcome-title "Are you better off renting or buying?"
  :welcome-mobile "Renting vs Buying"
  :welcome-desc "Deciding whether to rent or to buy can be difficult. There are so many factors in play that it’s easy to get lost trying to account for them all. Your income and savings are two important considerations, but so are how much you value permanence, how likely you are to want to stay in one place, and how confident you that your income won’t change. You can't put a number to many of these, yet they can all influence you decision.
                 What you can do, however, is estimate the financial impact of renting and buying. That’s what this tool is for. It’ll help you understand what you can afford, and what you should expect when you purchase or rent. It will also show you how this choice will play out over the coming years."
  :welcome-tooltip "Intro"
  :welcome-button "Let's get started"

  :people-title "Are you planning to purchase alone or jointly?"
  :people-mobile "One or two?"
  :people-desc "Buying together means you'll probably qualify for a larger mortgage, but both of you are now jointly liable for the loan—meaning if one of you fails to make a payment, the other must stand in. Defaulting on the loan also means that both your credit scores will be affected."
  :people-tooltip "Individual/Joint"
  :people-one "Individual Purchase"
  :people-two "Joint Purchase"

  :income-mobile "Income"
  :income-title-alone "How much do you make?"
  :income-title-together "How much do you & your partner make?"
  :income-desc "You income determines what you can afford. The taxes are estimated for the province of Quebec, but should be close enough for all Canadians. If you’re not from Canada, or you would like to bypass the tax calculations, you’ll need to enter your after-tax income."
  :income-tooltip "Income"

  :savings-mobile "Savings"
  :savings-title-alone "How much have you saved?"
  :savings-title-together "How much do you & your partner have saved?"
  :savings-desc "What you’ve saved also plays a major role in determining what you can afford. The minimal down payment in Canada is 5%. Additionally, for down payments of less than 20%, you will also have to purchase home default insurance. The down payment and the closing costs comprise the total cash required at purchase, and are evaluated in the next step."
  :savings-tooltip "Savings"

  :compare-mobile "Rent or Buy?"
  :compare-title "How much are you willing to spend?"
  :compare-desc " Your maximum home price is calculated from your income and your savings according to rules similar to those mortgage lenders use. The initial values below represent a housing cost of 25% of your gross monthly income, an often-recommended value.
The Land Transfer Tax is calculated for the municipality of Montreal (taxe de bienvenue), but should be similar in most other regions in Canada. The closing costs and additional fees can be controlled at the bottom of the page, in the assumptions section.  "
  :compare-tooltip "Rent or Buy?"

  :buying-desc "Below on the left is an estimate of the total cumulative cost for the next XX years should you purchase a home for YY or rent one for ZZ. On the right these costs are averaged monthly and graphed. The graph visualizes the financial benefits of purchasing or buying. You can click on a year to see the cost breakdown up until that point."
  :assumptions-desc "These assumptions are reasonable defaults for Montreal, Canada as calculated in May, 2015. You can edit or drag the sliders to adjust them to match your situation."

  :yearly-description "Real estate, inflation, and the returns on investments can be some of the trickiest things to predict, yet they can have a major impact on your financials down the line. The default values are sensible for the Canadian & Quebec economies, and are drawn from averages of the last 10 years. The investment rate of return is used to take into account the opportunity loss of higher payments. So, for example, when comparing a lower rent to a higher mortgage and large down payment, it assumes that you would have invested the difference."
  :mortgage-description "Your interest rate and term have a major influence on your immediate and future payments. Furthermore, you should expect to refinance your mortgage within 2 to 5 years, depending on your terms. This tool assumes a fixed-rate mortgage over the entire amortization period. While your finances won’t match this perfectly, it’s still a good estimate of what to expect in the future. The closing costs represent what you should expect to pay both when selling and purchasing a home. They usually vary between 1% and 4% of the selling price."
  :ownership-description "Maintenance and renovation fees often surprise many new home-buyers. You’ll need to inspect your home regularly, and repair parts that wear out. You’ll also have to take care of things such as snow removal and landscaping. If your property is a condo, some of these expenses may be included as part of your monthly maintenance fee. Property taxes depend on what type of property you own and where, but they can be estimated prior to purchase."

  ; ------

  ; input fields on the income page
  :my-income "I make"
  :partners-income "My partner makes"
  :yearly "per year"
  :monthly "per month"
  :biweekly "every 2 weeks"
  :before-tax "before tax"
  :after-tax "after tax"

  ; table headers in income page
  :income-top "Gross Income"
  :provincial-tax "Provincial Taxes"
  :federal-tax "Federal Taxex"
  :net-income "Net Income"
  :yearly-header "Yearly"
  :biweekly-header "Biweekly"

  ; table headers in mobile income page
  :income-top-mobile "Gross"
  :provincial-tax-mobile "Prov. Tax"
  :federal-tax-mobile "Fed. Tax"
  :net-income-mobile "Net"

  ; income final button
  :our-total-income "We make"
  :my-total-income "I make"
  :per-year-after-tax " per year after taxes."

  ; input fields on the savings page
  :my-savings "I have saved"
  :partners-savings "My partner has saved"
  :my-spend "and I'll spend"
  :partners-spend "and they'll spend"
  :my-save-percent "or"
  :partners-save-percent "or"
  :my-spend-percent "or"
  :partners-spend-percent "or"
  :of-my-income "of my income"
  :of-partners-income "of their income"
  :my-home "for the down payment."
  :our-home "for the down payment."
  :my-save "I want to save"
  :partners-save "My partner wants to save"

  ; savings table headers
  :year "Year"
  :saved-total "Total Savings"
  :saved-deinflated "Adjusted for Inflation"
  :with-m "With Home Purchase"
  :without-m "Without Home Purchase"

  ; savings final button
  :we-can-spend "We'll put down"
  :i-can-spend "I'll put down"
  :towards-our-home "towards our new home."
  :towards-my-home "towards my new home."

  :savings-disclaimer (str "Assuming " (format-percent (* 100 (assume :inflation)))
                           " annual inflation, a " (format-percent (* 100 (assume :rate-of-return)))
                          " rate of return, and a " (format-percent (* 100 (assume :income-growth)))
                          " yearly income growth.")

  ; compare sectiom
  :buy "Buy"
  :rent "Rent"
  :buying "Buying"
  :renting "Renting"

  :after "After"
  :years "years"

  :monthly-rent "Monthly Rent"

  :home-price "Home Price"
  :down-payment "Down Payment"
  :welcome-tax  "Land Transfer (Welcome) Tax"
  :closing-costs "Purchase Closing Costs"
  :selling-closing-costs "Selling Closing Costs"

  :mortgage-monthly-payment "Monthly Mortgage Payments"
  :monthly-maintainance "Monthly Maintainance Costs"

  :monthly-payment "Monthly Payment"

  :down ""
  :per-month "/month"

  :yearly-table "Total cost "
  :avg-monthly-graph "Average monthy cost"

  :renting-is-cheaper "Renting is cheaper"
  :buying-is-cheaper "Buying is cheaper"

  :initial "Initial"
  :recurring "Recurring"
  :opportunity "Opportunity"
  :selling "Sale of home"
  :total "Total"


  :by-the "By the"
  :is "is"
  :cheaper "cheaper"

  :cost-after "Cost after"

  :home-worth "Home value"
  :mortgage "Mortgage"
  :owner-value "You own"

  :is-cheaper-after "is cheaper after"
  :is-cheaper-for-next "is cheaper for the next"


  :additional-parameters "Assumptions"

  :yearly-increases "Yearly Growth"
  :home-growth "Home appreciation"
  :rent-growth "Yearly rent increase"
  :rate-of-return "Investment rate of return"
  :inflation "Inflation"

  :mortgage-details "Purchase Details"
  :mortgage-interest "Interest Rate"
  :amortization "Terms"

  :home-ownership-costs "Home Ownership Costs"
  :maintainance-fees "Maintainance & Renovations"
  :property-tax "Property Taxes"

  })


(defn copy [k]
  (get copy-english k (str "[" (name k) "]")))


(defn copy-p [k]
  (let [s  (get copy-english k (str "[" (name k) "]"))]
    (map #(vector :p (str %)) (string/split-lines s))))


(defn keywords-to-copy [ak]
  (apply assoc {} (interleave ak (map copy ak))))

(defn format-years [value]
  (str (round-to-0 value) " " (copy :years)))

