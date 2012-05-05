
;; BigDecimal comparisons do not appear to work as I would have
;; expected them to:

  (= 2e+3M 2000M)  ;; -> false

;; Looking at the definition of '=', I can see that it defers to
;; clojure.lang.Util/equiv, which compares them as Object, by
;; reference. Fine.


;; I thought I could use equals, but that one is incorrect too:

  (.equals 2e+3M 2000M)  ;; -> false

;; because the "scale" of these two objects is different, and
;; BigDecimal requires the scale to be the same:

  (map #(.scale %) [2e+3M 2000M]) ;; -> (-3 0)


;; Fine then; I'll use compare:

  (= 0 (compare 2e+3M 2000M)) ;; -> true

;; This still leaves me with a problem: what I really want to do
;; is compare data structures which contain instances of
;; BigDecimal:

  (= {:USD 2e+3M} {:USD 2000M}) ;; -> false, for the same
                                ;; reasons as above

  (compare {:USD 2e+3M} {:USD 2000M}) ;; PersistentArrayMap is
                                      ;; not Comparable

;; What is the idiomatic way to do this, in general?

;; Should I just give up and define a comparison method that
;; will work ONLY for the data structures I'm expecting to see?
;; (e.g. "maps with values that are all Comparable")


