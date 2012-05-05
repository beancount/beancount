(ns beancount.core
  (:use clojure.contrib.str-utils))



(def accounts (create-accounts :Debit
  '(Assets
    (Current
     (Cash)
     (RBC (Checking :CAD)
	  (Savings-US :USD)
	  (Savings :CAD))
     (HSBC (Checking :USD)
	   (Secured :USD)
	   (Savings :USD))
     (Condo (Propri-Gestion :CAD)
	    (Gesta-Conseil :CAD)))

    (Fixed (Home :CAD))
    (Loans (Mertz :USD)
	   (Filippo)

	   (AccountsReceivable)
	   (Points (AmericanAirlines :Miles)
		   (Aeroplan :Miles)
		   (WorldPerks :Miles)
		   (Quantas :Miles)
		   (FidoRewards :FidoDollars)
		   (Amtrak :Amtrak))
	   (Transfer))

    (Investments
     (RBCDirect
      (Taxable-US :USD
		  (QQQ) (RBF1003) (AAPL) (EFA) (EWJ) (GLD) (IEV [:IEV :IEV1]) (IJH) (ILF) (IVV) (IWM))
      (Taxable-CA :CAD
		  (CRA) (QQQ))
      (RSP-CA :CAD
	      (Contrib)
	      (JDU) (NT) (RHT) (AAPL)
	      (RBF550) (RBF551) (RBF558) (RBF559) (RBF556) (RBF564) (RBF468) (RBF265) (RBF462)
	      (RBF269) (NBC860) (RBF575) (RBF1002) (RBF1037) (RBF1016)
	      (AIS512) (AIS511)
	      (CSCO) (QQQ)
	      (XEG) (XMA [:XMA :XMA2]) (XIN) (XSU) (XSP) (IWM :IWM))))

    (HSBC-Securities :USD
		     (IVV :IVV))
    (OANDA :USD
	   (Primary :USD)
	   (Hedging :USD)
	   (Canadian :CAD))

    (London-Life-Policy :CAD)
    (Private
     (Furius)
     (Safehouse)))))

(defrecord Account [name comms children])

(defn accounts-parse-node [nodelist]
  (let [curs (filter keyword? nodelist)
	[name & children] (filter #(not (keyword? %)) nodelist)]
    (Account. name curs (map accounts-parse-node children))
    ))

(defn create-accounts [drcr root]
  (accounts-parse-node root))

(defn str-accounts [acc & [pfx]]
  (with-out-str
    (assert (identical? (type acc) Account))
    (let [pfx* (str-join ":" (remove nil? [pfx (:name acc)]))
	  children (:children acc)]
      (println (format "%s   (%s)" pfx* (str-join "," (:comms acc))))
      (dorun (map #(print-accounts % pfx*) children))
      )))

;(str-accounts accounts)




;;------------------------------------------------------------------------------

(defrecord Transaction [date
			flag
			description
			postings])

(defrecord Posting [account
		    amount
		    comm])
(defn make-posting [account
		    amount
		    comm]
  (Posting. account amount comm))


(defn register-transation [date flag description]
  (Transaction. date flag description [])
  )


(defn map* [f coll]
  "Like map, but applies to its arguments."
  (map (partial apply f) coll))

(defmacro tx [date flag description & postings]
  `(let [pos# (map* make-posting '~postings)]
       (Transaction. ~date ~flag ~description pos#)))



(println "--------------------------------------------------------------------------------")


[2010,06,19]


(tx [2010,06,19] \* "New Museum of Art | with Julie"
    (Expenses:Fun:Museum  10 USD)
    (Assets:Current:Cash -10 USD)
    )

(tx [2010,06,15] \* "Igor | haircut"
    (Expenses:Hair                 20 USD)
    (Assets:Current:Cash -20 USD))

(tx [2010,07,03] \* "magazine Time Out"
    (Assets:Current:Cash       -5 USD)
    (Expenses:Books 5 USD))









;; (ns serve (:use [swank.swank]))
;; (swank.swank/start-repl)
