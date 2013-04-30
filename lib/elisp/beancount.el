;;; beancount.el --- A minor mode that can be used to edit beancount input files.

;; Copyright (C) 2013 Martin Blais <blais@furius.ca>

;; This file is not part of GNU Emacs.

font-lock-defaults
font-lock-keywords

;;(org-font-lock-keywords t nil nil backward-paragraph)



(define-minor-mode beancount-mode
  "Toggle Beancount mode."
  :init-value nil
  :lighter " Beancount"
  ;; :keymap
  ;; '(([C-backspace] . hungry-electric-delete)
  ;;   ([C-M-backspace]
  ;;    . (lambda ()
  ;;        (interactive)
  ;;        (hungry-electric-delete t))))
  :group 'beancount

  (beancount-setup-font-lock-keywords beancount-mode)
  )

(defun beancount-setup-font-lock-keywords (addp)
  (if addp
      (font-lock-add-keywords major-mode beancount-font-lock-defaults)
      (font-lock-remove-keywords major-mode beancount-font-lock-defaults))
  (if font-lock-mode (font-lock-fontify-buffer)))

(defvar beancount-font-lock-defaults
  `(;; Strings
    ("\".*?\"" . font-lock-string-face)
    ;; Comments
    (";.+" . font-lock-comment-face)
    ;; Reserved keywords
    (,(regexp-opt '("txn" "check" "open" "close" "pad" "event" "price" "location" "note")) . font-lock-keyword-face)
    ;; Tags
    (,(regexp-opt '("begintag" "endtag")) . font-lock-keyword-face)
    ;; Date
    ("[0-9][0-9][0-9][0-9][-/][0-9][0-9][-/][0-9][0-9]" . font-lock-constant-face)
    ;; Account
    ("\\([A-Z][A-Za-z0-9\-]+:\\)+\\([A-Z][A-Za-z0-9\-]+\\)" . font-lock-builtin-face)
    ;; Number + Currency
    ("\\([0-9]+\\(\\.[0-9]+\\)?\\)\\s-+\\([A-Z][A-Z0-9'\.]\\{1,10\\}\\)" 1 bold)
    ;;("\\([0-9]+\\(\\.[0-9]+\\)?\\)\\s+\\([A-Z][A-Z0-9'\.]\\{1,10\\}\\)" . bold)
    ;; Txn Flags
    ("! " . font-lock-warning-face)
    ))



(provide 'beancount)
