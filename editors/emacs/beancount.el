;;; beancount.el --- A major mode to edit Beancount input files. -*- lexical-binding: t -*-

;; Copyright (C) 2013 Martin Blais <blais@furius.ca>
;; Copyright (C) 2015 Free Software Foundation, Inc.
;; Copyright (C) 2019 Daniele Nicolodi <daniele@grinta.net>

;; Version: 0
;; Author: Martin Blais <blais@furius.ca>
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Author: Daniele Nicolodi <daniele@grinta.net>

;; This file is not part of GNU Emacs.

;; This package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO: Add a flymake rule, using bean-check

;;; Code:

(autoload 'ido-completing-read "ido")
(require 'subr-x)

(defgroup beancount ()
  "Editing mode for Beancount files."
  :group 'beancount)

(defconst beancount-account-directive-names
  '("balance"
    "close"
    "document"
    "note"
    "open"
    "pad")
  "Directive bames that can appear after a date and are followd by an account.")

(defconst beancount-no-account-directive-names
  '("commodity"
    "event"
    "price"
    "query"
    "txn")
  "Directive names that can appear after a date and are _not_ followed by an account.")

(defconst beancount-timestamped-directive-names
  (append beancount-account-directive-names
          beancount-no-account-directive-names)
  "Directive names that can appear after a date.")

(defconst beancount-directive-names
  '("include"
    "option"
    "plugin"
    "poptag"
    "pushtag")
  "Directive names that can appear at the beginning of a line.")

(defvar beancount-directive-names
  (append beancount-directive-names
          beancount-timestamped-directive-names)
  "A list of the directive names.")

(defconst beancount-account-categories
  '("Assets" "Liabilities" "Equity" "Income" "Expenses"))

(defconst beancount-tag-chars "[:alnum:]-_/.")

(defconst beancount-account-chars "[:alnum:]-_:")

(defconst beancount-option-names
  ;; This list is kept in sync with the options defined in
  ;; beancount/parser/options.py.
  ;; Note: We should eventually build a tool that spits out the current list
  ;; automatically.
  '("title"
    "name_assets"
    "name_liabilities"
    "name_equity"
    "name_income"
    "name_expenses"
    "bookin_algorithm"
    "bookin_method"
    "account_previous_balances"
    "account_previous_earnings"
    "account_previous_conversions"
    "account_current_earnings"
    "account_current_conversions"
    "account_rounding"
    "conversion_currency"
    "inferred_tolerance_default"
    "inferred_tolerance_multiplier"
    "infer_tolerance_from_cost"
    "documents"
    "operating_currency"
    "render_commas"
    "plugin_processing_mode"
    "plugin"
    "long_string_maxlines"
    ))

(defconst beancount-date-regexp "[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]"
  "A regular expression to match dates.")

(defconst beancount-account-regexp
  (concat (regexp-opt beancount-account-categories)
          "\\(?::[[:upper:]][[:alnum:]-_]+\\)+")
  "A regular expression to match account names.")

(defconst beancount-number-regexp "[-+]?[0-9,]+\\(?:\\.[0-9]*\\)?"
  "A regular expression to match decimal numbers.")

(defconst beancount-currency-regexp "[A-Z][A-Z-_'.]*"
  "A regular expression to match currencies.")

(defconst beancount-flag-regexp
  ;; Single char taht is neither a space nor a lower-case letter.
  "[^ a-z]")

(defconst beancount-transaction-regexp
  (concat "^\\(" beancount-date-regexp "\\) +"
          "\\(?:txn +\\)?"
          "\\(" beancount-flag-regexp "\\) +"
          "\\(\".*\"\\)"))

(defconst beancount-posting-regexp
  (concat "^\\s-+"
          "\\(" beancount-account-regexp "\\)"
          "\\(?:\\s-+\\(\\(" beancount-number-regexp "\\)"
          "\\s-+\\(" beancount-currency-regexp "\\)\\)\\)?"))

(defconst beancount-directive-regexp
  (concat "^\\(" (regexp-opt beancount-directive-names) "\\) +"))

(defconst beancount-timestamped-directive-regexp
  (concat "^\\(" beancount-date-regexp "\\) +"
          "\\(" (regexp-opt beancount-timestamped-directive-names) "\\) +"))

(defconst beancount-metadata-regexp
  "^\\s-+\\([a-z][A-Za-z0-9]+:\\)\\s-+\\(.+\\)")

(defvar beancount-font-lock-keywords
  `(;; Reserved keywords
    (,(regexp-opt beancount-directive-names) . font-lock-keyword-face)

    ;; Tags & Links
    ("[#\\^][A-Za-z0-9\-_/.]+" . font-lock-type-face)

    ;; Date
    ("[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]" . font-lock-constant-face)

    ;; Account
    ("\\([A-Z][A-Za-z0-9\-]+:\\)+\\([A-Z0-9][A-Za-z0-9\-]*\\)" . font-lock-builtin-face)

    ;; Txn Flags
    ("! " . font-lock-warning-face)

    ;; Number + Currency
    ;;; ("\\([\\-+]?[0-9,]+\\(\\.[0-9]+\\)?\\)\\s-+\\([A-Z][A-Z0-9'\.]\\{1,10\\}\\)" . )
    ))


(defvar beancount-mode-map-prefix [(control c)]
  "The prefix key used to bind Beancount commands in Emacs")

(defvar beancount-mode-map
  (let ((map (make-sparse-keymap))
        (p beancount-mode-map-prefix))
    (define-key map (vconcat p [(\')]) #'beancount-insert-account)
    (define-key map (vconcat p [(control g)]) #'beancount-transaction-set-flag)
    (define-key map (vconcat p [(l)]) #'beancount-check)
    (define-key map (vconcat p [(q)]) #'beancount-query)
    (define-key map (vconcat p [(x)]) #'beancount-context)
    (define-key map (vconcat p [(k)]) #'beancount-linked)
    (define-key map (vconcat p [(p)]) #'beancount-insert-prices)
    (define-key map (vconcat p [(\;)]) #'beancount-align-to-previous-number)
    (define-key map (vconcat p [(\:)]) #'beancount-align-numbers)
    map))

(defvar beancount-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

;;;###autoload
(define-derived-mode beancount-mode fundamental-mode "Beancount"
  "A mode for Beancount files.

\\{beancount-mode-map}"
  :group 'beancount
  :syntax-table beancount-mode-syntax-table

  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)

  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+\\s-*")
  (setq-local comment-add 1)

  (setq-local indent-tabs-mode nil)

  (add-hook 'completion-at-point-functions #'beancount-completion-at-point nil t)

  (setq-local tab-always-indent 'complete)
  (setq-local completion-ignore-case t)

  (setq-local font-lock-defaults '(beancount-font-lock-keywords))
  (setq-local font-lock-syntax-table t))

(defun beancount-collect-pushed-tags (begin end)
  "Return list of all pushed (and not popped) tags in the region."
  (goto-char begin)
  (let ((tags (make-hash-table :test 'equal)))
    (while (re-search-forward
         (concat "^\\(push\\|pop\\)tag\\s-+\\(#[" beancount-tag-chars "]+\\)") end t)
      (if (string-equal (match-string 1) "push")
          (puthash (match-string-no-properties 2) nil tags)
        (remhash (match-string-no-properties 2) tags)))
    (hash-table-keys tags)))

(defun beancount-goto-transaction-begin ()
  "Move the cursor to the first line of the transaction definition."
  (interactive)
  (beginning-of-line)
  ;; everything that is indented with at lest one space or tab is part
  ;; of the transaction definition
  (while (looking-at-p "[ \t]+")
    (forward-line -1))
  (point))

(defun beancount-goto-transaction-end ()
  "Move the cursor to the line after the transaction definition."
  (interactive)
  (beginning-of-line)
  (if (looking-at-p beancount-transaction-regexp)
      (forward-line))
  ;; everything that is indented with at lest one space or tab as part
  ;; of the transaction definition
  (while (looking-at-p "[ \t]+")
    (forward-line))
  (point))

(defun beancount-goto-next-transaction (&optional arg)
  "Move to the next transaction.
With an argument move to the next non cleared transaction."
  (interactive "P")
  (beancount-goto-transaction-end)
  (let ((done nil))
    (while (and (not done)
                (re-search-forward beancount-transaction-regexp nil t))
      (if (and arg (string-equal (match-string 2) "*"))
          (goto-char (match-end 0))
        (goto-char (match-beginning 0))
        (setq done t)))
    (if (not done) (goto-char (point-max)))))

(defun beancount-find-transaction-extents (p)
  (save-excursion
    (goto-char p)
    (list (beancount-goto-transaction-begin)
          (beancount-goto-transaction-end))))

(defun beancount-inside-transaction-p ()
  (let ((bounds (beancount-find-transaction-extents (point))))
    (> (- (cadr bounds) (car bounds)) 0)))

(defun beancount-looking-at (regexp n pos)
  (and (looking-at regexp)
       (>= pos (match-beginning n))
       (<= pos (match-end n))))

(defvar beancount-accounts nil
  "A list of the accounts available in this buffer.")
(make-variable-buffer-local 'beancount-accounts)

(defun beancount-completion-at-point ()
  "Return the completion data relevant for the text at point."
  (save-excursion
    (save-match-data
      (let ((pos (point)))
        (beginning-of-line)
        (cond
         ;; non timestamped directive
         ((beancount-looking-at "[a-z]*" 0 pos)
          (list (match-beginning 0) (match-end 0)
                (mapcar (lambda (s) (concat s " ")) beancount-directive-names)))

         ;; poptag
         ((beancount-looking-at
           (concat "poptag\\s-+\\(\\(?:#[" beancount-tag-chars "]*\\)\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (beancount-collect-pushed-tags (point-min) (point))))

         ;; option
         ((beancount-looking-at
           (concat "^option\\s-+\\(\"[a-z_]*\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (mapcar (lambda (s) (concat "\"" s "\" ")) beancount-option-names)))

         ;; timestamped directive
         ((beancount-looking-at
           (concat beancount-date-regexp "\\s-+\\([[:alpha:]]*\\)") 1 pos)
          (list (match-beginning 1) (match-end 1)
                (mapcar (lambda (s) (concat s " ")) beancount-timestamped-directive-names)))

         ;; timestamped directives followed by account
         ((beancount-looking-at
           (concat "^" beancount-date-regexp
                   "\\s-+" (regexp-opt beancount-account-directive-names)
                   "\\s-+\\([" beancount-account-chars "]*\\)") 1 pos)
          (setq beancount-accounts nil)
          (list (match-beginning 1) (match-end 1) #'beancount-account-completion-table))

         ;; posting
         ((and (beancount-looking-at
                (concat "[ \t]+\\([" beancount-account-chars "]*\\)") 1 pos)
               ;; Do not force the account name to start with a
               ;; capital, so that it is possible to use substring
               ;; completion and we can rely on completion to fix
               ;; capitalization thanks to completion-ignore-case.
               (beancount-inside-transaction-p))
          (setq beancount-accounts nil)
          (list (match-beginning 1) (match-end 1) #'beancount-account-completion-table))

         ;; tags
         ((beancount-looking-at
           (concat "[ \t]+#\\([" beancount-tag-chars "]*\\)") 1 pos)
          (let* ((candidates nil)
                 (regexp (concat "\\#\\([" beancount-tag-chars "]+\\)"))
                 (completion-table
                  (lambda (string pred action)
                    (if (null candidates)
                        (setq candidates
                              (sort (delete string (beancount-collect regexp 1)) #'string<)))
                    (complete-with-action action candidates string pred))))
            (list (match-beginning 1) (match-end 1) completion-table)))

         ;; links
         ((beancount-looking-at
           (concat "[ \t]+\\^\\([" beancount-tag-chars "]*\\)") 1 pos)
          (let* ((candidates nil)
                 (regexp (concat "\\^\\([" beancount-tag-chars "]+\\)"))
                 (completion-table
                  (lambda (string pred action)
                    (if (null candidates)
                        (setq candidates
                              (sort (delete string (beancount-collect regexp 1)) #'string<)))
                    (complete-with-action action candidates string pred))))
            (list (match-beginning 1) (match-end 1) completion-table))))))))

(defun beancount-collect (regexp n)
  "Return an unique list of REGEXP group N in the current buffer."
  (save-excursion
    (save-match-data
      (let ((hash (make-hash-table :test 'equal)))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (puthash (match-string-no-properties n) nil hash))
        (hash-table-keys hash)))))

(defun beancount-account-completion-table (string pred action)
  (if (eq action 'metadata) '(metadata (category . beancount-account))
    (if (null beancount-accounts)
        (setq beancount-accounts
              (sort (delete string (beancount-collect beancount-account-regexp 0)) #'string<)))
    (complete-with-action action beancount-accounts string pred)))

;; Default to substring completion for beancount accounts.
(defconst beancount--completion-overrides
  '(beancount-account (styles basic partial-completion substring)))
(add-to-list 'completion-category-defaults beancount--completion-overrides)

(defcustom beancount-use-ido t
  "If non-nil, use ido-style completion rather than the standard completion."
  :type 'boolean)

(defun beancount-insert-account (account-name)
  "Insert one of the valid account names in this file.
Uses ido niceness according to `beancount-use-ido'."
  (interactive
   (list
    (if beancount-use-ido
        ;; `ido-completing-read' is too dumb to understand functional
        ;; completion tables!
        (ido-completing-read "Account: " beancount-accounts
                             nil nil (thing-at-point 'word))
      (completing-read "Account: " #'beancount-account-completion-table
                       nil t (thing-at-point 'word)))))
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))))
  (insert account-name))

(defun beancount-transaction-set-flag ()
  (interactive)
  (save-excursion
    (backward-paragraph 1)
    (forward-line 1)
    (while (search-forward "!" (line-end-position) t)
      (replace-match "*"))))

(defmacro beancount-for-line-in-region (begin end &rest exprs)
  "Iterate over each line in region until an empty line is encountered."
  `(save-excursion
     (let ((end-marker (copy-marker ,end)))
       (goto-char ,begin)
       (beginning-of-line)
       (while (and (not (eobp)) (< (point) end-marker))
         (beginning-of-line)
         (progn ,@exprs)
         (forward-line 1)
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
                                   beancount-currency-regexp)
                           line)
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
           (when (string-match (concat "^\\([^\"]*?\\)"
                                       "[ \t]+"
                                       "\\(" beancount-number-regexp "\\)"
                                       "[ \t]+"
                                       "\\(.*\\)$")
                               line)
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

(defvar beancount-install-dir nil
  "Directory in which Beancount's source is located.
Only useful if you have not installed Beancount properly in your PATH.")

(defvar beancount-check-program "bean-check"
  "Program to run to run just the parser and validator on an
  input file.")

(defvar compilation-read-command)

(defun beancount--run (prog &rest args)
  (let ((process-environment
         (if beancount-install-dir
             `(,(concat "PYTHONPATH=" beancount-install-dir)
               ,(concat "PATH="
                        (expand-file-name "bin" beancount-install-dir)
                        ":"
                        (getenv "PATH"))
               ,@process-environment)
           process-environment))
        (compile-command (mapconcat (lambda (arg)
                                      (if (stringp arg)
                                          (shell-quote-argument arg) ""))
                                    (cons prog args)
                                    " ")))
    (call-interactively 'compile)))

(defun beancount-check ()
  "Run `beancount-check-program'."
  (interactive)
  (let ((compilation-read-command nil))
    (beancount--run beancount-check-program
                    (file-relative-name buffer-file-name))))

(defvar beancount-query-program "bean-query"
  "Program to run to run just the parser and validator on an
  input file.")

(defun beancount-query ()
  "Run bean-query."
  (interactive)
  ;; Don't let-bind compilation-read-command this time, since the default
  ;; command is incomplete.
  (beancount--run beancount-query-program
                  (file-relative-name buffer-file-name) t))

(defvar beancount-doctor-program "bean-doctor"
  "Program to run the doctor commands.")

(defun beancount-context ()
  "Get the \"context\" from `beancount-doctor-program'."
  (interactive)
  (let ((compilation-read-command nil))
    (beancount--run beancount-doctor-program "context"
                    (file-relative-name buffer-file-name)
                    (number-to-string (line-number-at-pos)))))


(defun beancount-linked ()
  "Get the \"linked\" info from `beancount-doctor-program'."
  (interactive)
  (let ((compilation-read-command nil))
    (beancount--run beancount-doctor-program "linked"
                    (file-relative-name buffer-file-name)
                    (number-to-string (line-number-at-pos)))))

(defvar beancount-price-program "bean-price"
  "Program to run the price fetching commands.")

(defun beancount-insert-prices ()
  "Run bean-price on the current file and insert the output inline."
  (interactive)
  (call-process beancount-price-program nil t nil
                (file-relative-name buffer-file-name)))


(provide 'beancount)
;;; beancount.el ends here
