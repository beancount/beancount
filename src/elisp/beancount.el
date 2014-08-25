;;; beancount.el --- A minor mode that can be used to edit beancount input files.

;; Copyright (C) 2013 Martin Blais <blais@furius.ca>

;; This file is not part of GNU Emacs.

(require 'ido) ;; For completing read.
(require 'font-lock)


(define-minor-mode beancount-mode
  "A minor mode to help editing Beancount files.
This can be used within other text modes, in particular, org-mode
is great for sectioning large files with many transactions."
  :init-value nil
  :lighter " Beancount"
  :keymap
  '(
    ([(control c)(\')] . beancount-insert-account)
    ([(control c)(control g)] . beancount-transaction-set-flag)
    ([(control c)(r)] . beancount-init-accounts)
    ([(control c)(l)] . beancount-check)
    ([(control c)(\;)] . beancount-align-to-previous)
    ([(control c)(p)] . beancount-test-align)
    )
  :group 'beancount

  ;; The following is mostly lifted from lisp-mode.
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)

  (make-local-variable 'comment-start)
  (setq comment-start ";; ")
  (make-local-variable 'comment-start-skip)

  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (make-local-variable 'font-lock-comment-start-skip)
  ;; Font lock mode uses this only when it KNOWS a comment is starting.
  (setq font-lock-comment-start-skip ";+ *")
  (make-local-variable 'comment-add)
  (setq comment-add 1) ;; Default to `;;' in comment-region

  ;; No tabs by default.
  (set (make-local-variable 'indent-tabs-mode) nil)

  ;; Customize font-lock for beancount.
  ;;
  ;; Important: you have to use 'nil for the mode here because in certain major
  ;; modes (e.g. org-mode) the font-lock-keywords is a buffer-local variable.
  (if beancount-mode
      (font-lock-add-keywords nil beancount-font-lock-defaults)
    (font-lock-remove-keywords nil beancount-font-lock-defaults))
  (font-lock-fontify-buffer)

  (when beancount-mode
    (make-variable-buffer-local 'beancount-accounts)
    (beancount-init-accounts))
  )


(defun beancount-init-accounts ()
  "Initialize or reset the list of accounts."
  (interactive)
  (setq beancount-accounts (beancount-get-accounts))
  (message "Accounts updated."))


(defvar beancount-font-lock-defaults
  `(;; Comments
    (";.+" . font-lock-comment-face)

    ;; Strings
    ("\".*?\"" . font-lock-string-face)

    ;; Reserved keywords
    (,(regexp-opt '("txn" "balance" "open" "close" "pad" "event" "price" "note" "document"
                    "pushtag" "poptag")) . font-lock-keyword-face)

    ;; Tags & Links
    ("[#\\^][A-Za-z0-9\-_/.]+" . font-lock-type-face)

    ;; Date
    ("[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]" . font-lock-constant-face)

    ;; Account
    ("\\([A-Z][A-Za-z0-9\-]+:\\)+\\([A-Z][A-Za-z0-9\-]+\\)" . font-lock-builtin-face)

    ;; Txn Flags
    ("! " . font-lock-warning-face)

    ;; Number + Currency
    ;;; ("\\([\\-+]?[0-9]+\\(\\.[0-9]+\\)?\\)\\s-+\\([A-Z][A-Z0-9'\.]\\{1,10\\}\\)" . )

    ))


(defvar beancount-date-regexp "[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]"
  "A regular expression to match dates.")

(defvar beancount-account-regexp (concat (regexp-opt '("Assets"
                                                       "Liabilities"
                                                       "Equity"
                                                       "Income"
                                                       "Expenses"))
                                         "\\(?::[A-Z][A-Za-z0-9-_:]+\\)")
  "A regular expression to match account names.")

(defvar beancount-number-regexp "[-+]?[0-9\\.]+"
  "A regular expression to match decimal numbers in beancount.")

(defvar beancount-currency-regexp "[A-Z][A-Z-_'.]*"
  "A regular expression to match currencies in beancount.")


(defvar beancount-accounts nil
  "A list of the accounts available in this buffer. This is a
  cache of the value computed by beancount-get-accounts.")


(defun beancount-hash-keys (hashtable)
  "Extract all the keys of the given hashtable. Return a sorted list."
  (let (rlist)
    (maphash (lambda (k v) (push k rlist)) hashtable)
    rlist))


(defun beancount-get-accounts ()
  "Heuristically obtain a list of all the accounts used in all the postings.
We ignore patterns seen the line 'exclude-line'. If ALL is non-nil, look
for account names in postings as well (default is to look at the @defaccount
declarations only."
  (let ((accounts (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward beancount-account-regexp nil t)
        (puthash (match-string-no-properties 0) nil accounts)))
    (sort (beancount-hash-keys accounts) 'string<)))


(defun beancount-insert-account (account-name)
  "Insert one of the valid account names in this file (using ido
niceness)."
  (interactive
   (list (ido-completing-read "Account: " beancount-accounts nil nil (thing-at-point 'word))))
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))))
  (insert account-name))


(defun beancount-transaction-set-flag ()
  (interactive)
  (save-excursion
    (backward-paragraph 1)
    (forward-line 1)
    (replace-string "!" "*" nil (line-beginning-position) (line-end-position))))


(defmacro beancount-for-line-in-region (begin end &rest exprs)
  "Iterate over each line in region until an empty line is encountered."
  `(save-excursion
     (let ((end-marker (set-marker (make-marker) ,end)))
       (goto-char ,begin)
       (forward-line 1)
       (beginning-of-line)
       (while (< (point) end-marker)
         (progn ,@exprs)
         (forward-line 1)
         (beginning-of-line)
         ))))


(defun beancount-max-accounts-width (begin end)
  "Return the minimum widths of a list of account names on a list
of lines. Initial whitespace is ignored."
  (let* (widths)
  (beancount-for-line-in-region
   begin end
   (let* ((line (thing-at-point 'line)))
     (when (string-match
            (concat "^[ \t]*\\(" beancount-account-regexp "\\)") line)
       (push (length (match-string 1 line)) widths))))
   (apply 'max widths)))


(defun beancount-align-postings (begin end &optional requested-currency-column)
  "Align all postings in the given region. CURRENCY-COLUMN is the character
at which to align the beginning of the amount's currency."
  (interactive "r")
  (let* ((number-width 12)
         (min-currency-column
          (max requested-currency-column
               ;; spc account spc number
               (+ 2 (beancount-max-accounts-width begin end) 1 number-width)
               )))
    (beancount-for-line-in-region
     begin end
     (let* ((line (thing-at-point 'line))
            (number-format
             (format " %%%ss %%s" number-width))
            (account-format
             (format "  %%-%ss" (- min-currency-column 2 (+ 1 number-width 1)))))
       (when (string-match
              (concat "^[ \t]+"
                      "\\(?:\\(.\\)[ \t]+\\)?"
                      "\\([A-Z][A-Za-z0-9_-]+:[A-Za-z0-9_:\\-]+\\)"
                      "[ \t]+"
                      "\\(?:\\([-+]?[0-9.]+\\)[ \t]+\\(.*\\)\\)")
              line)
         (delete-region (line-beginning-position) (line-end-position))
         (let* ((flag (match-string 1 line))
                (account (match-string 2 line))
                (flag-and-account
                 (if flag
                     (format "%s %s" flag account)
                   (format "%s" account)))
                (number (match-string 3 line))
                (rest (match-string 4 line)) )
           (insert (format account-format flag-and-account))
           (when (and number rest)
             (insert (format number-format number rest)))))))))


(defun beancount-align-to-previous ()
  "Align postings under the point's paragraph.
This function looks for a posting in the previous transaction to
determine the column at which to align the transaction, or otherwise
the fill column, and align all the postings of this transaction to
this column."
  (interactive)
  (let* ((begin (save-excursion
                  (beancount-beginning-of-directive)
                  (point)))
         (end (save-excursion
                (goto-char begin)
                (forward-paragraph 1)
                (point)))
         (currency-column (or (beancount-find-previous-alignment-column)
                              fill-column)))
    (beancount-align-postings begin end currency-column)))


(defun beancount-beginning-of-directive ()
  "Move point to the beginning of the enclosed or preceding directive."
  (while (and (> (point) (point-min))
              (not (looking-at
                      "[0-9][0-9][0-9][0-9][\-/][0-9][0-9][\-/][0-9][0-9]")))
    (forward-line -1)))


(defun beancount-find-previous-alignment-column ()
  "Find the preceding column to align amounts with.
This is used to align transactions at the same column as that of
the previous transaction in the file. This function merely finds
what that column is and returns it (an integer)."
  ;; Go hunting for the last column with a suitable posting.
  (let (column)
    (save-excursion
      ;; Go to the beginning of the enclosing directive.
      (beancount-beginning-of-directive)
      (forward-line -1)

      ;; Find the last posting with an amount and a currency on it.
      (let ((posting-regexp (concat
                             "\\s-+"
                             beancount-account-regexp "\\s-+"
                             beancount-number-regexp "\\s-+"
                             "\\(" beancount-currency-regexp "\\)"))
            (balance-regexp (concat
                             beancount-date-regexp "\\s-+"
                             "balance" "\\s-+"
                             beancount-account-regexp "\\s-+"
                             beancount-number-regexp "\\s-+"
                             "\\(" beancount-currency-regexp "\\)")))
        (while (and (> (point) (point-min))
                    (not (or (looking-at posting-regexp)
                             (looking-at balance-regexp))))
          (forward-line -1))
        (when (or (looking-at posting-regexp)
                  (looking-at balance-regexp))
          (setq column (- (match-beginning 1) (point))))
        ))
    column))


(defvar beancount-check-program "bean-check"
  "Program to run to run just the parser and validator on an
  input file.")

(defun beancount-check ()
  (interactive)
  (let ((compilation-read-command nil)
        (compile-command
         (format "%s %s" beancount-check-program (buffer-file-name))))
    (call-interactively 'compile)))


(provide 'beancount)


;; FIXME: beancount-align-to-current-line
;; FIXME: Support aligning balance and price directives


;; A sample file to test printing the context around a transaction with.



;; FIXME: Bug: cursor on @ below, it fetches to far behind on align
;;
;; 2014-01-01 open Income:Magic
;; 2014-01-01 open Assets:Checking
;; 2014-01-01 open Assets:Investments:Cash
;; 2014-01-01 open Assets:Investments:SMTG
;; 2014-01-01 open Expenses:Commissions
;; 2014-01-01 open Income:Investments:PnL
;;
;; 2014-07-10 * "Transfer"
;;   Income:Magic  6000.00 USD
;;   Assets:Checking
;;
;; 2014-07-12 * "More to investing account"
;;   Assets:Checking                         -5000 USD
;;   Assets:Investments:Cash                    10 USD
;;
;; 2014-07-15 * "Buy some shares"
;;   Assets:Investments:Cash                  -4000 USD
;;   Assets:Investments:SMTG                     40 SMTG {100 USD}
;;
;; 2014-08-17 balance Assets:Investments:Cash          1000.01 @USD
;;
