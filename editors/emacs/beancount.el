;;; beancount.el --- A minor mode that can be used to edit beancount input files.

;; Copyright (C) 2013 Martin Blais <blais@furius.ca>
;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Version: 0
;; Author: Martin Blais <blais@furius.ca>
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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
(require 'font-lock)

(defgroup beancount ()
  "Editing mode for Beancount files."
  :group 'beancount)

(defconst beancount-timestamped-directive-names
  '("balance"
    "open"
    "close"
    "pad"
    "document"
    "note"
    ;; The ones below are not followed by an account name.
    "event"
    "price"
    "commodity"
    "query"
    "txn")
  "Directive names that can appear after a date.")

(defconst beancount-nontimestamped-directive-names
  '("pushtag"
    "poptag"
    "option"
    "include"
    "plugin")
  "Directive names that can appear after a date.")

(defvar beancount-directive-names
  (append beancount-nontimestamped-directive-names
          beancount-timestamped-directive-names)
  "A list of the directive names.")

(defconst beancount-tag-chars "[:alnum:]-_/.")

(defconst beancount-account-categories
  '("Assets" "Liabilities" "Equity" "Income" "Expenses"))

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

(defvar beancount-font-lock-keywords
  `(;; Reserved keywords
    (,(regexp-opt beancount-directive-names) . font-lock-keyword-face)

    ;; Tags & Links
    ("[#\\^][A-Za-z0-9\-_/.]+" . font-lock-type-face)

    ;; Date
    ("[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]" . font-lock-constant-face)

    ;; Account
    ("\\([A-Z][A-Za-z0-9\-]+:\\)+\\([A-Z][A-Za-z0-9\-]+\\)" . font-lock-builtin-face)

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
    (define-key map (vconcat p [(r)]) #'beancount-init-accounts)
    (define-key map (vconcat p [(l)]) #'beancount-check)
    (define-key map (vconcat p [(q)]) #'beancount-query)
    (define-key map (vconcat p [(x)]) #'beancount-context)
    (define-key map (vconcat p [(k)]) #'beancount-linked)
    (define-key map (vconcat p [(p)]) #'beancount-insert-prices)
    (define-key map (vconcat p [(\;)]) #'beancount-align-to-previous-number)
    (define-key map (vconcat p [(\:)]) #'beancount-align-numbers)

    ;; FIXME: Binding TAB breaks expected org-mode behavior to fold/unfold. We
    ;; need to find a better solution.
    ;;(define-key map [?\t] #'beancount-tab)
    map))

(defvar beancount-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"\"" st)
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defun beancount--goto-bob () (goto-char (point-min)))

;;;###autoload
(define-minor-mode beancount-mode
  "A minor mode to help editing Beancount files.
This can be used within other text modes, in particular, org-mode
is great for sectioning large files with many transactions.

\\{beancount-mode-map}"
  :init-value nil
  :lighter " Beancount"
  :group 'beancount

  ;; The following is mostly lifted from lisp-mode.

  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) #'lisp-fill-paragraph)

  (set (make-local-variable 'comment-start) ";; ")

  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  ;; Font lock mode uses this only when it KNOWS a comment is starting.
  ;; FIXME: Why bother?
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  ;; Default to `;;' in comment-region.
  (set (make-local-variable 'comment-add) 1)

  ;; Org-mode sets both of these to `org-comment-or-uncomment-region',
  ;; which doesn't know about our ";" comments.
  (kill-local-variable 'comment-region-function)
  (kill-local-variable 'uncomment-region-function)

  ;; No tabs by default.
  (set (make-local-variable 'indent-tabs-mode) nil)

  (add-hook 'completion-at-point-functions
            #'beancount-completion-at-point nil t)
  (set (make-local-variable 'completion-ignore-case) t)

  ;; Customize font-lock for beancount.
  ;;
  (set-syntax-table beancount-mode-syntax-table)
  (when (fboundp 'syntax-ppss-flush-cache)
    (syntax-ppss-flush-cache (point-min))
    (set (make-local-variable 'syntax-begin-function) #'beancount--goto-bob))
  ;; Force font-lock to use the syntax-table to find strings-and-comments,
  ;; regardless of what the "host major mode" decided.
  (set (make-local-variable 'font-lock-keywords-only) nil)
  ;; Important: you have to use 'nil for the mode here because in certain major
  ;; modes (e.g. org-mode) the font-lock-keywords is a buffer-local variable.
  (if beancount-mode
      (font-lock-add-keywords nil beancount-font-lock-keywords)
    (font-lock-remove-keywords nil beancount-font-lock-keywords))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (with-no-warnings (font-lock-fontify-buffer)))

  (when beancount-mode
    (beancount-init-accounts))
  )

(defvar beancount-accounts nil
  "A list of the accounts available in this buffer.
This is a cache of the value computed by `beancount-get-accounts'.")
(make-variable-buffer-local 'beancount-accounts)

(defun beancount-init-accounts ()
  "Initialize or reset the list of accounts."
  (interactive)
  (setq beancount-accounts (beancount-get-accounts))
  (message "Accounts updated."))

(defvar beancount-date-regexp "[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]"
  "A regular expression to match dates.")

(defvar beancount-account-regexp
  (concat (regexp-opt beancount-account-categories)
          "\\(?::[[:upper:]][" beancount-account-chars "]+\\)")
  "A regular expression to match account names.")

(defvar beancount-number-regexp "[-+]?[0-9,]+\\(?:\\.[0-9]*\\)?"
  "A regular expression to match decimal numbers in beancount.")

(defvar beancount-currency-regexp "[A-Z][A-Z-_'.]*"
  "A regular expression to match currencies in beancount.")

(defun beancount-tab ()
  "Try to use the right meaning of TAB."
  (interactive)
  (let ((cdata (beancount-completion-at-point)))
    (if cdata
        ;; There's beancount-specific completion at point.
        (call-interactively #'completion-at-point)
      (let* ((beancount-mode nil)
             (fallback (key-binding (this-command-keys))))
        (if (commandp fallback)
            (command-execute fallback))))))

(defun beancount-tags (prefix)
  "Return list of all tags starting with PREFIX in current buffer.
Excludes tags appearing on the current line."
  (unless (string-match "\\`[#^]" prefix)
    (error "Unexpected prefix to search tags: %S" prefix))
  (let ((found ())
        (re (concat prefix "[" beancount-tag-chars "]*")))
    (save-excursion
      (forward-line 0)
      (while (re-search-backward re nil t)
        (push (match-string 0) found)))
    ;; Ignore tags on current line.
    (save-excursion
      (forward-line 1)
      (while (re-search-forward re nil t)
        (push (match-string 0) found)))
    (delete-dups found)))

(defconst beancount-txn-regexp
  ;; For the full definition of a flag, see the rule that emits FLAG in
  ;; beancount/parser/lexer.l. For this, let's assume that it's a single char
  ;; that's neither a space nor a lower-case letter. This should be updated as
  ;; the parser is improved.
  "^[0-9-/]+ +\\(?:txn +\\)?[^ [:lower:]]\\($\\| \\)")

(defun beancount-inside-txn-p ()
  ;; FIXME: The doc doesn't actually say how the legs of a transaction can be
  ;; layed out.  We assume that they all start with some space on the line.
  (save-excursion
    (forward-line 0)
    (while (and (looking-at "[ \t]") (not (bobp)))
      (forward-line -1))
    (looking-at beancount-txn-regexp)))

(defun beancount-completion-at-point ()
  "Return the completion data relevant for the text at point."
  (let ((bp (buffer-substring (line-beginning-position) (point))))
    (cond
     ((string-match "\\`[a-z]*\\'" bp)
      ;; A directive starting at BOL (hence non-timestamped).
      (list (line-beginning-position)
            (save-excursion (skip-chars-forward "a-z") (point))
            '("pushtag" "poptag")))

     ((string-match
       (concat "\\`option +\\(\"[a-z_]*\\)?\\'")
       bp)
      (list (- (point)
               (if (match-end 1) (- (match-end 1) (match-beginning 1)) 0))
            (save-excursion (skip-chars-forward "a-z_")
                            (if (looking-at "\"") (forward-char 1))
                            (point))
            (mapcar (lambda (s) (concat "\"" s "\"")) beancount-option-names)))

     ((string-match
       (concat "\\`poptag +\\(#[" beancount-tag-chars "]*\\)?\\'")
       bp)
      (list (- (point)
               (if (match-end 1) (- (match-end 1) (match-beginning 1)) 0))
            (save-excursion (skip-chars-forward beancount-tag-chars) (point))
            (save-excursion
              (let ((opened ()))
                (while (re-search-backward
                        (concat "^pushtag +\\(#[" beancount-tag-chars "]+\\)")
                        nil t)
                  (push (match-string 1) opened))
                opened))))

     ((string-match "\\`[0-9-/]+ +\\([[:alpha:]]*\\'\\)" bp)
      ;; A timestamped directive.
      (list (- (point) (- (match-end 1) (match-beginning 1)))
            (save-excursion (skip-chars-forward "[:alpha:]") (point))
            beancount-timestamped-directive-names))

     ((and (beancount-inside-txn-p)
           (string-match (concat "\\`[ \t]+\\(["
                                 beancount-account-chars "]*\\)\\'")
                         bp))
      ;; Hopefully, an account name.  We don't force the partially-written
      ;; account name to start with a capital, so that it's possible to use
      ;; substring completion and also so we can rely on completion to put the
      ;; right capitalization (thanks to completion-ignore-case).
      (list (- (point) (- (match-end 1) (match-beginning 1)))
            (save-excursion (skip-chars-forward beancount-account-chars)
                            (point))
            #'beancount-account-completion-table))

     ((string-match (concat "\\`[0-9-/]+ +\\("
                            (regexp-opt beancount-timestamped-directive-names)
                            "\\) +\\([" beancount-account-chars "]*\\'\\)")
                    bp)
      (list (- (point) (- (match-end 2) (match-beginning 2)))
            (save-excursion (skip-chars-forward beancount-account-chars)
                            (point))
            (if (equal (match-string 1 bp) "open")
                (append
                 (mapcar (lambda (c) (concat c ":")) beancount-account-categories)
                 beancount-accounts)
              #'beancount-account-completion-table)))

     ((string-match (concat "[#^][" beancount-tag-chars "]*\\'") bp)
      (list (- (point) (- (match-end 0) (match-beginning 0)))
            (save-excursion (skip-chars-forward beancount-tag-chars) (point))
            (completion-table-dynamic #'beancount-tags))))))

(defun beancount-hash-keys (hashtable)
  "Extract all the keys of the given hashtable. Return a sorted list."
  (let (rlist)
    (maphash (lambda (k _v) (push k rlist)) hashtable)
    (sort rlist 'string<)))

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

(defcustom beancount-use-ido t
  "If non-nil, use ido-style completion rather than the standard completion."
  :type 'boolean)

(defun beancount-account-completion-table (string pred action)
  (if (eq action 'metadata)
      '(metadata (category . beancount-account))
    (complete-with-action action beancount-accounts string pred)))

;; Default to substring completion for beancount accounts.
(defconst beancount--completion-overrides
  '(beancount-account (styles basic partial-completion substring)))
(cond
 ((boundp 'completion-category-defaults)
  (add-to-list 'completion-category-defaults beancount--completion-overrides))
 ((and (boundp 'completion-category-overrides)
       (not (assq 'beancount-account completion-category-overrides)))
  (push beancount--completion-overrides completion-category-overrides)))

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
