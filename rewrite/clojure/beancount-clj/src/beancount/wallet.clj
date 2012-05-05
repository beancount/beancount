(ns beancount.wallet)


(def roundmap [var {}]
     "A mapping of commodities to their precision, as inferred from the file.")


(defn make-wallet
  "Create a new mapping of currency to amount."
  (with-meta {} {:class 'Wallet}))

(defn mask-wallet [other]

  ({} [:a 1 :b 2])

)

