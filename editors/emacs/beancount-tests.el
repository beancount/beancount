;;; beancount-test.el --- ERT for beancount-mode

;; Copyright 2019 Daniele Nicolodi <daniele@grinta.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.


(require 'ert)
(require 'beancount)

(ert-deftest beancount/smoke-001 ()
  :tags '(regress)
  (with-temp-buffer
    (beancount-mode)
    (font-lock-ensure)))

(defun beancount-test-fontify-string (string)
  "Fontify STRING in beancount-mode."
  (with-temp-buffer
    (insert string)
    (beancount-mode)
    (font-lock-ensure)
    (buffer-string)))

(defun beancount-test-face-groups (fontified)
  "Group a fontified string by face.
Return a list of substrings each followed by its face."
  (cl-loop for start = 0 then end
           while start
           for end   = (next-single-property-change start 'face fontified)
           for prop  = (get-text-property start 'face fontified)
           for text  = (substring-no-properties fontified start end)
           if prop
           append (list text prop)))

(defun beancount-test-group-str-by-face (str)
  "Fontify `str' in beancount-mode and group it by face.
Return a list of substrings each followed by its face."
  (beancount-test-face-groups (beancount-test-fontify-string str)))

(defun beancount-test-font-lock (source face-groups)
  "Test that `source' fontifies to the expected `face-groups'."
  (should (equal (beancount-test-group-str-by-face source) face-groups)))

(ert-deftest beancount/fontify-001 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 * \"Example\"
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "*"                beancount-narrative-cleared
     "\"Example\""      beancount-narrative-cleared
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-002 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 ! \"Example\"
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "!"                beancount-narrative-pending
     "\"Example\""      beancount-narrative-pending
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-003 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 A \"Example\"
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "A"                beancount-narrative
     "\"Example\""      beancount-narrative
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-004 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 * \"Example\"
  #foo
  ^bar
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "*"                beancount-narrative-cleared
     "\"Example\""      beancount-narrative-cleared
     "#foo"             beancount-tag
     "^bar"             beancount-link
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))
