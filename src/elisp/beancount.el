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
    ([(control c)(q)] . beancount-query)
    ([(control c)(\;)] . beancount-align-to-previous-number)
    ([(control c)(\:)] . beancount-align-numbers)
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


(defun beancount-align-numbers (begin end &optional requested-currency-column)
  "Align all numbers in the given region. CURRENCY-COLUMN is the character
at which to align the beginning of the amount's currency. If not specified, use
the smallest columns that will align all the numbers.  With a prefix argument,
align with the fill-column."
  (interactive "r")

  ;; With a prefix argument, align with the fill-column.
  (when current-prefix-arg
    (setq requested-currency-column fill-column))

  ;; Loop once in the region to find the length of the longest string before the
  ;; number.
  (let (prefix-widths
        number-widths
        (number-padding "  "))
    (beancount-for-line-in-region
     begin end
     (let ((line (thing-at-point 'line)))
       (when (string-match (concat "\\(.*?\\)"
                                   "[ \t]+"
                                   "\\(" beancount-number-regexp "\\)"
                                   "[ \t]+"
                                   beancount-currency-regexp) line)
         (push (length (match-string 1 line)) prefix-widths)
         (push (length (match-string 2 line)) number-widths)
         )))

    (when prefix-widths
      ;; Loop again to make the adjustments to the numbers.
      (let* ((number-width (apply 'max number-widths))
             (number-format (format "%%%ss" number-width))
             ;; Compute rightmost column of prefix.
             (max-prefix-width (apply 'max prefix-widths))
             (max-prefix-width
              (if requested-currency-column
                  (max (- requested-currency-column (length number-padding) number-width 1)
                       max-prefix-width)
                max-prefix-width))
             (prefix-format (format "%%-%ss" max-prefix-width))
             )

        (beancount-for-line-in-region
         begin end
         (let ((line (thing-at-point 'line)))
           (when (string-match (concat "\\(.*?\\)"
                                       "[ \t]+"
                                       "\\(" beancount-number-regexp "\\)"
                                       "[ \t]+"
                                       "\\(.*\\)$") line)
             (delete-region (line-beginning-position) (line-end-position))
             (let* ((prefix (match-string 1 line))
                    (number (match-string 2 line))
                    (rest (match-string 3 line)) )
               (insert (format prefix-format prefix))
               (insert number-padding)
               (insert (format number-format number))
               (insert " ")
               (insert rest)))))))))


(defun beancount-align-to-previous-number ()
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
    (beancount-align-numbers begin end currency-column)))


(defun beancount-beginning-of-directive ()
  "Move point to the beginning of the enclosed or preceding directive."
  (beginning-of-line)
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


(defvar beancount-query-program "bean-query"
  "Program to run to run just the parser and validator on an
  input file.")

(defun beancount-query ()
  (interactive)
  (let ((compile-command
         (format "%s %s " beancount-query-program (buffer-file-name))))
    (call-interactively 'compile)))


(provide 'beancount)
