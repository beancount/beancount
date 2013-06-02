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
  '(([(control c)(\')] . beancount-insert-account))
  :group 'beancount

  (set (make-local-variable 'comment-start) ";; ")
  (set (make-local-variable 'comment-end) "")

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
  (setq beancount-accounts (beancount-get-accounts)))


;; font-lock-builtin-face 	font-lock-comment-delimiter-face
;; font-lock-comment-face 	font-lock-constant-face
;; font-lock-doc-face 	font-lock-function-name-face
;; font-lock-keyword-face 	font-lock-negation-char-face
;; font-lock-preprocessor-face 	font-lock-reference-face
;; font-lock-string-face 	font-lock-syntactic-face-function
;; font-lock-type-face 	font-lock-variable-name-face
;; font-lock-warning-face

(defvar beancount-font-lock-defaults
  `(;; Comments
    (";.+" . font-lock-comment-face)

    ;; Strings
    ("\".*?\"" . font-lock-string-face)

    ;; Reserved keywords
    (,(regexp-opt '("txn" "check" "open" "close" "pad" "event" "price" "note"
                    "begintag" "endtag")) . font-lock-keyword-face)

    ;; Tags
    ("#[A-Za-z0-9\-_/.]+" . font-lock-type-face)

    ;; Date
    ("[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]" . font-lock-constant-face)

    ;; Account
    ("\\([A-Z][A-Za-z0-9\-]+:\\)+\\([A-Z][A-Za-z0-9\-]+\\)" . font-lock-builtin-face)

    ;; Txn Flags
    ("! " . font-lock-warning-face)

    ;; Number + Currency
    ;;; ("\\([\\-+]?[0-9]+\\(\\.[0-9]+\\)?\\)\\s-+\\([A-Z][A-Z0-9'\.]\\{1,10\\}\\)" . )

    ))

(defvar beancount-account-regexp (concat (regexp-opt '("Assets"
                                                       "Liabilities"
                                                       "Equity"
                                                       "Income"
                                                       "Expenses"))
                                         "\\(:[A-Z][A-Za-z0-9-_:]+\\)")
  "A regular expression to match account names.")

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


(provide 'beancount)
