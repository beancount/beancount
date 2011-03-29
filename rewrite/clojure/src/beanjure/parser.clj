(ns beanjure.parser
  (:use [clojure.contrib.repl-utils :only (show)]
        [clojure.contrib.duck-streams :only (read-lines)]
        [clojure.pprint :only (pprint)])

  (:require [clojure.java.io :as jio])
  )

;;------------------------------------------------------------------------------

(def r-comment #"^[ \t]*;(.*)$")

(defn skip-line? [line]
  (or (re-matches r-comment line)
      (re-matches #"^[ \t]*$" line)))

;;------------------------------------------------------------------------------

(def r-date #"(\d\d\d\d)[/-](\d\d)[/-](\d\d)")

; A date within a note.
(def r-effdate (re-pattern (format "^%s(=%s)?" r-date r-date)))

;;------------------------------------------------------------------------------

(def r-directive #"^@([a-z_]+)\s+([^;]*)(?:;.*)?")

(defn parse-directive [acc m]
  (let [[_ dirtype contents] m]
    (condp = dirtype
	"defaccount" (parse-defaccount acc m)
	"var" (parse-accid acc m)
	"begintag" [:begintag]
	"endtag" [:endtag]
	nil)))

;;------------------------------------------------------------------------------

(defrecord Account [kind name])

(def r-account #"[A-Z][A-Za-z0-9-]+(?::[A-Z][A-Za-z0-9-]*)*")

(def r-defaccount
  (re-pattern (format "(Cr|Dr)[ \t]+(%s)(?:[ \t]+(.*))?" r-account)))

(defn parse-defaccount [acc m]
  (if-let [mm (re-matches r-defaccount (nth m 2))]
    (new Account (nth mm 1) (keyword (nth mm 2)))
    (throw (Exception. (str "Invalid line:" (first m))))
    ))

;;------------------------------------------------------------------------------

(defrecord AccID [account id])

(def r-accid (re-pattern
	      (format "ofx[ \t]+accid[ \t]+([0-9A-Z-]+)[ \t]+(%s)" r-account)))

(defn parse-accid [acc m]
  (if-let [mm (re-matches r-accid (nth m 2))]
    (new AccID (keyword (nth mm 2)) (nth mm 1))
    (throw (Exception. (str "Invalid line:" (first m))))
    ))

;;------------------------------------------------------------------------------

(defrecord Transaction [date effdate flag payee description postings])

(def r-txn (re-pattern
	    (format "^%s[ \t]+(?:(.)[ \t]+)?(\\(.*?\\))?(.*)$" r-effdate)))

(defn parse-txn [lines acc]
  (let [line (first lines), rest (next lines)]
    (println "  txn" line)
    (if (nil? line)
      acc
      (if (skip-line? line)
	(fn [] (parse-top rest acc))
	(fn [] (parse-top rest acc)))
;; FIXME: here do something
	;; (fn [] (parse-top rest (assoc-list acc :txns (new Transaction nil nil nli nil nil nil)))))
	;; (fn [] (parse-txn rest acc))))
    )))

    ;; postaccount_re = re.compile('(?:%(accname)s|\[%(accname)s\]|\(%(accname)s\))' %
    ;;                             {'accname': account_re.pattern})

    ;; # Pattern for a posting line (part of a transaction).
    ;; posting_re = re.compile(
    ;;     ('\s+([*!]\s+)?(%(account)s)' # account name
    ;;      '(?:'
    ;;      '(?:\s+%(amount)s)?'  # main
    ;;      '(?:\s+(?:({)\s*%(amount)s\s*}|({{)\s*%(amount)s\s*}}))?' # declared cost
    ;;      '(?:\s+@(@?)(?:\s+%(amount)s))?\s*(?:;(.*))?\s*$'
    ;;      '|'
    ;;      '\s+(BOOK)\s+%(commodity)s(?:\s+(IN)\s+%(commodity)s)?\s*$'  # booking entry
    ;;      ')') %  # price/note
    ;;     {'amount': amount_re.pattern,
    ;;      'account': postaccount_re.pattern,
    ;;      'commodity': commodity_re.pattern})

;; # Pattern for a transaction line.
;; txn_re = re.compile('^%(date)s(=%(date)s)?\s+(?:(.)\s+)?(\(.*?\))?(.*)$' %
;;                     {'date': date_re.pattern})
;; payee_sep = ' | '
;; desc_re = re.compile('(?:\s*([^|]+?)\s*\|)?\s*([^|]*?)\s*$')

    ;; # Pattern for an amount.
    ;; commodity_re = re.compile('"?([A-Za-z][A-Za-z0-9.~\']*)"?')
    ;; amount_re = re.compile('([-+]?\d*(?:\.\d*)?)\s+%(comm)s' %
    ;;                        {'comm': commodity_re.pattern})



;;------------------------------------------------------------------------------

(defn parse-top [lines acc]
  (let [line (first lines), rest (next lines)]
    (if (nil? line)
      acc
      (if (skip-line? line)
	(fn [] (parse-top rest acc))

	(if-let [m (re-matches r-txn line)]
	  (fn [] (parse-txn rest acc))

	  (if-let [m (re-matches r-directive line)]
	    (let [directive (parse-directive acc m)]
	      (fn [] (parse-top rest (if directive
				       (assoc-list acc :directives directive)
				       acc))))

	    (fn [] (parse-top rest acc))
	    ))))))

;;------------------------------------------------------------------------------

(defn assoc-list [map key value]
  (assoc map key (conj (get map key []) value)))

(defn read-ledger [filename]
  (trampoline parse-top (take 400 (read-lines filename)) {})
  )

(comment

(let [f (str (System/getenv "HOME") "/q/accounting/blais.ledger")]
  (pprint (read-ledger f)))

  )



;; Improvmenets:
;;   remove 'var ofx accid' -> @accid
;;   @check becomes -> @check-after @check-before
;;   only tags on postings, no notes vs. tags
;;   a new Exercise object; all Ledger objects serve Exercise
;;   Balsheet begin
;;   import should support bayesian

