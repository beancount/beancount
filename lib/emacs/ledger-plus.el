;;; ledger-plus.el --- Additions to ledger.el for future integration.

;; Copyright (C) 2008 Martin Blais <blais@furius.ca>

;;; Commentary:

(require 'ledger)

(defun ledger-accounts nil
  "The list of accounts in the current buffer.")
(make-variable-buffer-local 'ledger-accounts)

(add-hook 'ledger-mode-hook 'ledger-plus-hook-function)

(defun ledger-insert-date ()
  "A simpler, less smart, more convenient date insertion function
that just inserts today's date."
  (interactive)
  (insert
   (time-stamp-string "%:y/%02m/%02d * ")
   ))

(defun ledger-plus-hook-function ()
  "Hook to install on entering ledger mode."

  ;; Setup partial completion delimiters.
  (make-local-variable 'PC-word-delimiters)
  (setq PC-word-delimiters ": ")

  (define-key ledger-mode-map [(control ?c) (control ?d)] 'ledger-insert-date)
  (define-key ledger-mode-map [(control ?c) (control ?i)] 'ledger-insert-account)

  )

(defun ledger-insert-account ()
  "Prompts the user for an account to insert at point."
  (interactive)
  (let ((accounts (ledger-get-accounts (line-number-at-pos (point)))))
    (insert
     (ledger-completing-read "Account: " accounts))))

(require 'iswitchb)
(defun ledger-completing-read (prompt choices)
  "(I can't get icomplete nor icicles to work nicely, this is
old, solid and lovely.)"
  (let ((iswitchb-make-buflist-hook
         (lambda ()
           (setq iswitchb-temp-buflist choices)))
	(iswitchb-case t))
    (iswitchb-read-buffer prompt)))



;; FIXME: add history too.




;; (defun my-icompleting-read (prompt choices)
;;   "Use iswitch as a completing-read replacement to choose from
;; choices.  PROMPT is a string to prompt with.  CHOICES is a list of
;; strings to choose from."
;;   (let ((iswitchb-make-buflist-hook
;;          (lambda ()
;;            (setq iswitchb-temp-buflist choices))))
;;     (iswitchb-read-buffer prompt)))






;;; 	(skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
;;; 	;; Don't delete formfeeds, even if they are considered whitespace.
;;; 	(save-match-data
;;; 	  (if (looking-at ".*\f")
;;; 	      (goto-char (match-end 0))))
;;; 	(delete-region (point) (match-end 0))))))


(defun ledger-get-accounts (exclude-line)
  "Heuristically obtain a list of all the accounts used in all the postings.
We ignore patterns seen the line 'exclude-line'."
  (let ((accounts))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "\\(?:"
		      "^[ \t]+\\([A-Z][A-Za-z0-9-_:]*\\)\\s-*"
		      "\\|"
		      "^@defaccount\\s-+\\(?:De\\|Cr\\)\\s-+\\([A-Z][A-Za-z0-9-_:]*\\)\\s-*"
		      "\\)"
		      ) nil t)
	(let ((no (if (match-string 1) 1 2)))
	  (when (not (= (line-number-at-pos (match-end no)) exclude-line))
	    (let ((acc (match-string no)))
	      (add-to-list 'accounts acc)
	      )))))
    accounts))



;; The following two variables are used to implement cycling between
;; accounts.
(defvar ledger-last-account-matches nil
  "The list of account names last matched by an invocation of
ledger-expand-account.")

(defun ledger-expand-account ()
  "Look at the word before point and try to expand it into an account name."
  (interactive)
  ;; Note: we use 'filename as a thing, because it accepts the : separators for
  ;; the underlying words we're looking for.
  (let ((w (thing-at-point 'filename))
	(bounds (bounds-of-thing-at-point 'filename)))

    (if (and repeatable-repeated
	     ledger-last-account-matches)

	;; Use the previous search string.
	(let ((newmatch (or
			 (cadr (member w ledger-last-account-matches))
			 (car ledger-last-account-matches))))
	  (when newmatch
	    (kill-region (car bounds) (cdr bounds))
	    (insert newmatch)))

      ;; Do the search.
      (let ((regexp (format ".*%s.*" w))
	    ;; A regexp that would place the match in first priority.
	    (regexp-prio (format ".*\\(\b%s\\|%s\b\\)" w w))
	    (accounts (ledger-get-accounts (line-number-at-pos (point)))))

	;; Filter the list of matches by our word's regexp.
	(setq ledger-last-account-matches

	      ;; Sort the list by preferring those matches which match the
	      ;; word's boundary first.
	      (mapcar 'cdr
		      (sort

		       (mapcar  ;; Schwartzian transform.
			(lambda (x) (cons (string-match regexp-prio x)
					  x))

			;; Filter in only those accounts that match the word.
			(filter (lambda (x) (string-match regexp x))
				accounts))

		       ;; Comparison that takes into account the boundary match.
		       (lambda (x y)
			 (let ((px (car x))
			       (py (car y)))
			   (if (and px py)
			       (string< (cdr x) (cdr y))
			     (if px t nil))))

		       )))

	(cond ((= (length ledger-last-account-matches) 1)
	       (kill-region (car bounds) (cdr bounds))
	       (insert (car ledger-last-account-matches)))

	      ((> (length ledger-last-account-matches) 1)
	       (message
		(concat (format "Many matches for %s: " w)
			(mapconcat 'identity ledger-last-account-matches ", "))
		)
	       (kill-region (car bounds) (cdr bounds))
	       (insert (car ledger-last-account-matches))
	       )

	      (t (message (format "No matches for '%s'." w)))
	      ))

      )))

;; FIXME: in the sorting order, we should look at the accounts used in the file,
;; right before our line number. Actually, sort the original account definitions
;; according to the distance from the current line.




(define-key ledger-mode-map [(control ?c) (?')] 'ledger-expand-account)

;; Allow cycling through the matching names.
(when (require 'repeatable nil t)
  (repeatable-command-advice ledger-expand-account))


;; Make emacs able to go to the errors generated in our Python-code. With a
;; suitable logging.basicConfig(), this should work for other programs too.

(unless (assq 'python-logging compilation-error-regexp-alist-alist)

  (add-to-list
   'compilation-error-regexp-alist-alist
   '(python-logging "\\(WARNING\\|ERROR\\|CRITICAL\\)\\s-*:\\s-*\\(.+\\):\\([0-9]+\\)\\s-*:" 2 3))

  (add-to-list
   'compilation-error-regexp-alist 'python-logging)

  )



(provide 'ledger-plus)

;;; ledger-plus.el ends here
