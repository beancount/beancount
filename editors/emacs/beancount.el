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
(require 'outline)

(defgroup beancount ()
  "Editing mode for Beancount files."
  :group 'beancount)

(defcustom beancount-transaction-indent 2
  "Transaction indent."
  :type 'integer
  :group 'beancount)

(defcustom beancount-number-alignment-column 52
  "Column to which align numbers in postinng definitions. Set to
0 to automatically determine the minimum column that will allow
to align all amounts."
  :type 'integer
  :group 'beancount)

(defcustom beancount-highlight-transaction-at-point nil
  "If t highlight transaction under point."
  :type 'boolean
  :group 'beancount)

(defcustom beancount-use-ido t
  "If non-nil, use ido-style completion rather than the standard."
  :type 'boolean
  :group 'beancount)

(defgroup beancount-faces nil "Beancount mode highlighting" :group 'beancount)

(defface beancount-directive
  `((t :inherit font-lock-keyword-face))
  "Face for Beancount directives."
  :group 'beancount-faces)

(defface beancount-tag
  `((t :inherit font-lock-type-face))
  "Face for Beancount tags."
  :group 'beancount-faces)

(defface beancount-link
  `((t :inherit font-lock-type-face))
  "Face for Beancount links."
  :group 'beancount-faces)

(defface beancount-date
  `((t :inherit font-lock-constant-face))
  "Face for Beancount dates."
  :group 'beancount-faces)

(defface beancount-account
  `((t :inherit font-lock-builtin-face))
  "Face for Beancount account names."
  :group 'beancount-faces)

(defface beancount-amount
  `((t :inherit font-lock-default-face))
  "Face for Beancount amounts."
  :group 'beancount-faces)

(defface beancount-narrative
  `((t :inherit font-lock-builtin-face))
  "Face for Beancount transactions narrative."
  :group 'beancount-faces)

(defface beancount-narrative-cleared
  `((t :inherit font-lock-string-face))
  "Face for Beancount cleared transactions narrative."
  :group 'beancount-faces)

(defface beancount-narrative-pending
  `((t :inherit font-lock-keyword-face))
  "Face for Beancount pending transactions narrative."
  :group 'beancount-faces)

(defface beancount-metadata
  `((t :inherit font-lock-type-face))
  "Face for Beancount metadata."
  :group 'beancount-faces)

(defface beancount-highlight
  `((t :inherit highlight))
  "Face to highlight Beancount transaction at point."
  :group 'beancount-faces)

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
  '("account_current_conversions"
    "account_current_earnings"
    "account_previous_balances"
    "account_previous_conversions"
    "account_previous_earnings"
    "account_rounding"
    "allow_deprecated_none_for_tags_and_links"
    "allow_pipe_separator"
    "booking_method"
    "conversion_currency"
    "documents"
    "infer_tolerance_from_cost"
    "inferred_tolerance_default"
    "inferred_tolerance_multiplier"
    "insert_pythonpath"
    "long_string_maxlines"
    "name_assets"
    "name_equity"
    "name_expenses"
    "name_income"
    "name_liabilities"
    "operating_currency"
    "plugin_processing_mode"
    "render_commas"
    "title"))

(defconst beancount-date-regexp "[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}"
  "A regular expression to match dates.")

(defconst beancount-account-regexp
  (concat (regexp-opt beancount-account-categories)
          "\\(?::[[:upper:]][[:alnum:]-_]+\\)+")
  "A regular expression to match account names.")

(defconst beancount-number-regexp "[-+]?[0-9]+\\(?:,[0-9]\\{3\\}\\)*\\(?:\\.[0-9]*\\)?"
  "A regular expression to match decimal numbers.")

(defconst beancount-currency-regexp "[A-Z][A-Z-_'.]*"
  "A regular expression to match currencies.")

(defconst beancount-flag-regexp
  ;; Single char that is neither a space nor a lower-case letter.
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

(defvar beancount-outline-regexp "\\(;;;+\\|\\*+\\)")

(defun beancount-outline-level ()
  (let ((len (- (match-end 1) (match-beginning 1))))
    (if (equal (substring (match-string 1) 0 1) ";")
        (- len 2)
      len)))

(defun beancount-face-by-state (state)
  (cond ((string-equal state "*") 'beancount-narrative-cleared)
        ((string-equal state "!") 'beancount-narrative-pending)
        (t 'beancount-narrative)))

(defun beancount-outline-face ()
  (if outline-minor-mode
      (cl-case (funcall outline-level)
      (1 'org-level-1)
      (2 'org-level-2)
      (3 'org-level-3)
      (4 'org-level-4)
      (5 'org-level-5)
      (6 'org-level-6)
      (otherwise nil))
    nil))

(defvar beancount-font-lock-keywords
  `((,beancount-transaction-regexp (1 'beancount-date)
                                   (2 (beancount-face-by-state (match-string 2)) t)
                                   (3 (beancount-face-by-state (match-string 2)) t))
    (,beancount-posting-regexp (1 'beancount-account)
                               (2 'beancount-amount nil :lax))
    (,beancount-metadata-regexp (1 'beancount-metadata)
                                (2 'beancount-metadata t))
    (,beancount-directive-regexp (1 'beancount-directive))
    (,beancount-timestamped-directive-regexp (1 'beancount-date)
                                             (2 'beancount-directive))
    ;; Fontify section headers when composed with outline-minor-mode.
    (,(concat "^\\(" beancount-outline-regexp "\\).*") . (0 (beancount-outline-face)))
    ;; Tags and links.
    (,(concat "\\#[" beancount-tag-chars "]*") . 'beancount-tag)
    (,(concat "\\^[" beancount-tag-chars "]*") . 'beancount-link)
    ;; Number followed by currency not covered by previous rules.
    (,(concat beancount-number-regexp "\\s-+" beancount-currency-regexp) . 'beancount-amount)
    ;; Accounts not covered by previous rules.
    (,beancount-account-regexp . 'beancount-account)
    ))

(defun beancount-tab-dwim (&optional arg)
  (interactive "P")
  (if (and outline-minor-mode
           (or arg (outline-on-heading-p)))
      (beancount-outline-cycle arg)
    (indent-for-tab-command)))

(defvar beancount-mode-map-prefix [(control c)]
  "The prefix key used to bind Beancount commands in Emacs")

(defvar beancount-mode-map
  (let ((map (make-sparse-keymap))
        (p beancount-mode-map-prefix))
    (define-key map (kbd "TAB") #'beancount-tab-dwim)
    (define-key map (vconcat p [(\')]) #'beancount-insert-account)
    (define-key map (vconcat p [(control g)]) #'beancount-transaction-clear)
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
  (setq-local fill-paragraph-function #'beancount-indent-transaction)

  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+\\s-*")
  (setq-local comment-add 1)

  (setq-local indent-line-function #'beancount-indent-line)
  (setq-local indent-region-function #'beancount-indent-region)
  (setq-local indent-tabs-mode nil)

  (setq-local tab-always-indent 'complete)
  (setq-local completion-ignore-case t)
  
  (add-hook 'completion-at-point-functions #'beancount-completion-at-point nil t)
  (add-hook 'post-command-hook #'beancount-highlight-transaction-at-point nil t)
  
  (setq-local font-lock-defaults '(beancount-font-lock-keywords))
  (setq-local font-lock-syntax-table t)

  (setq-local outline-regexp beancount-outline-regexp)
  (setq-local outline-level #'beancount-outline-level))

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
  ;; everything that is indented with at least one space or tab as part
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

(defun beancount-number-alignment-column ()
  "Return the column to which postings amounts should be aligned to.
Returns `beancount-number-alignment-column' unless it is 0. In
that case, scan the buffer to determine the minimum column that
will allow to align all numbers."
  (if (> beancount-number-alignment-column 0)
      beancount-number-alignment-column
    (save-excursion
      (save-match-data
        (let ((account-width 0)
              (number-width 0))
          (goto-char (point-min))
          (while (re-search-forward beancount-posting-regexp nil t)
            (if (match-string 2)
                (let ((accw (- (match-end 1) (line-beginning-position)))
                      (numw (- (match-end 3) (match-beginning 3))))
                  (setq account-width (max account-width accw)
                        number-width (max number-width numw)))))
          (+ account-width 2 number-width))))))

(defun beancount-compute-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Only timestamped directives start with a digit.
     ((looking-at-p "[0-9]") 0)
     ;; Otherwise look at the previous line.
     ((and (= (forward-line -1) 0)
           (or (looking-at-p "[ \t].+")
               (looking-at-p beancount-timestamped-directive-regexp)
               (looking-at-p beancount-transaction-regexp)))
      beancount-transaction-indent)
     ;; Default.
     (t 0))))

(defun beancount-align-number (target-column)
  (save-excursion
    (beginning-of-line)
    ;; Check if the current line is a posting with a number to align.
    (when (and (looking-at beancount-posting-regexp)
               (match-string 2))
      (let* ((account-end-column (- (match-end 1) (line-beginning-position)))
             (number-width (- (match-end 3) (match-beginning 3)))
             (account-end (match-end 1))
             (number-beginning (match-beginning 3))
             (spaces (max 2 (- target-column account-end-column number-width))))
        (unless (eq spaces (- number-beginning account-end))
          (goto-char account-end)
          (delete-region account-end number-beginning)
          (insert (make-string spaces ? )))))))

(defun beancount-indent-line ()
  (let ((indent (beancount-compute-indentation))
        (savep (> (current-column) (current-indentation))))
    (unless (eq indent (current-indentation))
      (if savep (save-excursion (indent-line-to indent))
        (indent-line-to indent)))
    (unless (eq this-command 'beancount-tab-dwim)
      (beancount-align-number (beancount-number-alignment-column)))))

(defun beancount-indent-region (start end)
  "Indent a region automagically. START and END specify the region to indent."
  (let ((deactivate-mark nil)
        (beancount-number-alignment-column (beancount-number-alignment-column)))
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (unless (looking-at-p "\\s-*$")
          (beancount-indent-line))
        (forward-line 1))
      (move-marker end nil))))

(defun beancount-indent-transaction (&optional _justify _region)
  "Indent Beancount transaction at point."
  (interactive)
  (save-excursion
    (let ((bounds (beancount-find-transaction-extents (point))))
      (beancount-indent-region (car bounds) (cadr bounds)))))

(defun beancount-transaction-clear (&optional arg)
  "Clear transaction at point. With a prefix argument set the
transaction as pending."
  (interactive "P")
  (save-excursion
    (save-match-data
      (let ((flag (if arg "!" "*")))
        (beancount-goto-transaction-begin)
        (if (looking-at beancount-transaction-regexp)
            (replace-match flag t t nil 2))))))

(defun beancount-insert-account (account-name)
  "Insert one of the valid account names in this file.
Uses ido niceness according to `beancount-use-ido'."
  (interactive
   (list
    (if beancount-use-ido
        ;; `ido-completing-read' does not understand functional
        ;; completion tables thus directly build a list of the
        ;; accounts in the buffer
        (let ((beancount-accounts
               (sort (beancount-collect beancount-account-regexp 0) #'string<)))
          (ido-completing-read "Account: " beancount-accounts
                               nil nil (thing-at-point 'word)))
      (completing-read "Account: " #'beancount-account-completion-table
                       nil t (thing-at-point 'word)))))
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))))
  (insert account-name))

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

;;; Transaction highligh

(defvar beancount-highlight-overlay (list))
(make-variable-buffer-local 'beancount-highlight-overlay)

(defun beancount-highlight-overlay-make ()
  (let ((overlay (make-overlay 1 1)))
    (overlay-put overlay 'face 'beancount-highlight)
    (overlay-put overlay 'priority '(nil . 99))
    overlay))

(defun beancount-highlight-transaction-at-point ()
  "Move the highlight overlay to the current transaction."
  (when beancount-highlight-transaction-at-point
    (unless beancount-highlight-overlay
      (setq beancount-highlight-overlay (beancount-highlight-overlay-make)))
    (let* ((bounds (beancount-find-transaction-extents (point)))
           (begin (car bounds))
           (end (cadr bounds)))
      (if (> (- end begin) 0)
          (move-overlay beancount-highlight-overlay begin end)
        (move-overlay beancount-highlight-overlay 1 1)))))

;;; Outline minor mode support.

(defun beancount-outline-cycle (&optional arg)
  "Implement visibility cycling a la `org-mode'.

The behavior of this command is determined by the first matching
condition among the following:

 1. When point is at the beginning of the buffer, or when called
    with a `\\[universal-argument]' universal argument, rotate the entire buffer
    through 3 states:

   - OVERVIEW: Show only top-level headlines.
   - CONTENTS: Show all headlines of all levels, but no body text.
   - SHOW ALL: Show everything.

 2. When point is at the beginning of a headline, rotate the
    subtree starting at this line through 3 different states:

   - FOLDED:   Only the main headline is shown.
   - CHILDREN: The main headline and its direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.

   - SUBTREE:  Show the entire subtree, including body text."
  (interactive "P")
  (setq deactivate-mark t)
  (cond
   ;; Beginning of buffer or called with C-u: Global cycling
   ((or (equal arg '(4))
        (and (bobp)
             ;; org-mode style behaviour - only cycle if not on a heading
             (not (outline-on-heading-p))))
    (beancount-cycle-buffer))

   ;; At a heading: rotate between three different views
   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    (outline-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; First, some boundaries
      (save-excursion
        (save-excursion (beancount-next-line) (setq eol (point)))
        (outline-end-of-heading)              (setq eoh (point))
        (outline-end-of-subtree)              (setq eos (point)))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
        ;; Nothing is hidden behind this heading
        (beancount-message "EMPTY ENTRY"))
       ((>= eol eos)
        ;; Entire subtree is hidden in one line: open it
        (outline-show-entry)
        (outline-show-children)
        (beancount-message "CHILDREN")
        (setq
         this-command 'beancount-cycle-children))
       ((eq last-command 'beancount-cycle-children)
        ;; We just showed the children, now show everything.
        (outline-show-subtree)
        (beancount-message "SUBTREE"))
       (t
        ;; Default action: hide the subtree.
        (outline-hide-subtree)
        (beancount-message "FOLDED")))))))

(defvar beancount-current-buffer-visibility-state nil
  "Current visibility state of buffer.")
(make-variable-buffer-local 'beancount-current-buffer-visibility-state)

(defvar beancount-current-buffer-visibility-state)

(defun beancount-cycle-buffer (&optional arg)
  "Rotate the visibility state of the buffer through 3 states:
  - OVERVIEW: Show only top-level headlines.
  - CONTENTS: Show all headlines of all levels, but no body text.
  - SHOW ALL: Show everything.

With a numeric prefix ARG, show all headlines up to that level."
  (interactive "P")
  (save-excursion
    (cond
     ((integerp arg)
      (outline-show-all)
      (outline-hide-sublevels arg))
     ((eq last-command 'beancount-cycle-overview)
      ;; We just created the overview - now do table of contents
      ;; This can be slow in very large buffers, so indicate action
      ;; Visit all headings and show their offspring
      (goto-char (point-max))
      (while (not (bobp))
        (condition-case nil
            (progn
              (outline-previous-visible-heading 1)
              (outline-show-branches))
          (error (goto-char (point-min)))))
      (beancount-message "CONTENTS")
      (setq this-command 'beancount-cycle-toc
            beancount-current-buffer-visibility-state 'contents))
     ((eq last-command 'beancount-cycle-toc)
      ;; We just showed the table of contents - now show everything
      (outline-show-all)
      (beancount-message "SHOW ALL")
      (setq this-command 'beancount-cycle-showall
            beancount-current-buffer-visibility-state 'all))
     (t
      ;; Default action: go to overview
      (let ((toplevel
             (cond
              (current-prefix-arg
               (prefix-numeric-value current-prefix-arg))
              ((save-excursion
                 (beginning-of-line)
                 (looking-at outline-regexp))
               (max 1 (funcall outline-level)))
              (t 1))))
        (outline-hide-sublevels toplevel))
      (beancount-message "OVERVIEW")
      (setq this-command 'beancount-cycle-overview
            beancount-current-buffer-visibility-state 'overview)))))

(defun beancount-message (msg)
  "Display MSG, but avoid logging it in the *Messages* buffer."
  (let ((message-log-max nil))
    (message msg)))

(defun beancount-next-line ()
  "Forward line, but mover over invisible line ends.
Essentially a much simplified version of `next-line'."
  (interactive)
  (beginning-of-line 2)
  (while (and (not (eobp))
              (get-char-property (1- (point)) 'invisible))
    (beginning-of-line 2)))

(provide 'beancount)
;;; beancount.el ends here
