;;; ledger.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2007 John Wiegley (johnw AT gnu DOT org)

;; Emacs Lisp Archive Entry
;; Filename: ledger.el
;; Version: 2.6.1
;; Date: Thu 16-Apr-2007
;; Keywords: data
;; Author: John Wiegley (johnw AT gnu DOT org)
;; Maintainer: John Wiegley (johnw AT gnu DOT org)
;; Description: Helper code for using my "ledger" command-line tool
;; URL: http://www.newartisans.com/johnw/emacs.html
;; Compatibility: Emacs22

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; To use this module: Load this file, open a ledger data file, and
;; type M-x ledger-mode.  Once this is done, you can type:
;;
;;   C-c C-a       add a new entry, based on previous entries
;;   C-c C-e       toggle cleared status of an entry
;;   C-c C-y       set default year for entry mode
;;   C-c C-m       set default month for entry mode
;;   C-c C-r       reconcile uncleared entries related to an account
;;   C-c C-o C-r   run a ledger report
;;   C-C C-o C-g   goto the ledger report buffer
;;   C-c C-o C-e   edit the defined ledger reports
;;   C-c C-o C-s   save a report definition based on the current report
;;   C-c C-o C-a   rerun a ledger report
;;   C-c C-o C-k   kill the ledger report buffer
;;
;; In the reconcile buffer, use SPACE to toggle the cleared status of
;; a transaction, C-x C-s to save changes (to the ledger file as
;; well), or C-c C-r to attempt an auto-reconcilation based on the
;; statement's ending date and balance.
;;
;; The ledger reports command asks the user to select a report to run
;; then creates a report buffer containing the results of running the
;; associated command line.  Its' behavior is modified by a prefix
;; argument which, when given, causes the generated command line that
;; will be used to create the report to be presented for editing
;; before the report is actually run.  Arbitrary unnamed command lines
;; can be run by specifying an empty name for the report.  The command
;; line used can later be named and saved for future use as a named
;; report from the generated reports buffer.
;;
;; In a report buffer, the following keys are available:
;;   (space)  scroll up
;;   e        edit the defined ledger reports
;;   s        save a report definition based on the current report
;;   q        quit the report (return to ledger buffer)
;;   r        redo the report
;;   k        kill the report buffer

(require 'esh-util)
(require 'esh-arg)
(require 'pcomplete)

(defvar ledger-version "1.2"
  "The version of ledger.el currently loaded")

(defgroup ledger nil
  "Interface to the Ledger command-line accounting program."
  :group 'data)

(defcustom ledger-binary-path (executable-find "ledger")
  "Path to the ledger executable."
  :type 'file
  :group 'ledger)

(defcustom ledger-clear-whole-entries nil
  "If non-nil, clear whole entries, not individual transactions."
  :type 'boolean
  :group 'ledger)

(defcustom ledger-reports
  '(("bal" "ledger -f %(ledger-file) bal")
    ("reg" "ledger -f %(ledger-file) reg")
    ("payee" "ledger -f %(ledger-file) reg -- %(payee)")
    ("account" "ledger -f %(ledger-file) reg %(account)"))
  "Definition of reports to run.

Each element has the form (NAME CMDLINE).  The command line can
contain format specifiers that are replaced with context sensitive
information.  Format specifiers have the format '%(<name>)' where
<name> is an identifier for the information to be replaced.  The
`ledger-report-format-specifiers' alist variable contains a mapping
from format specifier identifier to a lisp function that implements
the substitution.  See the documentation of the individual functions
in that variable for more information on the behavior of each
specifier."
  :type '(repeat (list (string :tag "Report Name")
		       (string :tag "Command Line")))
  :group 'ledger)

(defcustom ledger-report-format-specifiers
  '(("ledger-file" . ledger-report-ledger-file-format-specifier)
    ("payee" . ledger-report-payee-format-specifier)
    ("account" . ledger-report-account-format-specifier))
  "Alist mapping ledger report format specifiers to implementing functions

The function is called with no parameters and expected to return the
text that should replace the format specifier."
  :type 'alist
  :group 'ledger)

(defcustom ledger-default-acct-transaction-indent "    "
  "Default indentation for account transactions in an entry."
  :type 'string
  :group 'ledger)

(defvar bold 'bold)
(defvar ledger-font-lock-keywords
  `((,(concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
	      "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") 3 bold)
    (";.+" . font-lock-comment-face)
    ("^\\s-+.+?\\(  \\|\t\\|\n\\|\\s-+$\\)" . font-lock-keyword-face))
  "Default expressions to highlight in Ledger mode.")

(defsubst ledger-current-year ()
  (format-time-string "%Y"))
(defsubst ledger-current-month ()
  (format-time-string "%m"))

(defvar ledger-year (ledger-current-year)
  "Start a ledger session with the current year, but make it
customizable to ease retro-entry.")
(defvar ledger-month (ledger-current-month)
  "Start a ledger session with the current month, but make it
customizable to ease retro-entry.")

(defun ledger-iterate-entries (callback)
  (goto-char (point-min))
  (let* ((now (current-time))
	 (current-year (nth 5 (decode-time now))))
    (while (not (eobp))
      (when (looking-at
	     (concat "\\(Y\\s-+\\([0-9]+\\)\\|"
		     "\\([0-9]\\{4\\}+\\)?[./]?"
		     "\\([0-9]+\\)[./]\\([0-9]+\\)\\s-+"
		     "\\(\\*\\s-+\\)?\\(.+\\)\\)"))
	(let ((found (match-string 2)))
	  (if found
	      (setq current-year (string-to-number found))
	    (let ((start (match-beginning 0))
		  (year (match-string 3))
		  (month (string-to-number (match-string 4)))
		  (day (string-to-number (match-string 5)))
		  (mark (match-string 6))
		  (desc (match-string 7)))
	      (if (and year (> (length year) 0))
		  (setq year (string-to-number year)))
	      (funcall callback start
		       (encode-time 0 0 0 day month
				    (or year current-year))
		       mark desc)))))
      (forward-line))))

(defun ledger-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun ledger-time-subtract (t1 t2)
  "Subtract two time values.
Return the difference in the format of a time value."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun ledger-find-slot (moment)
  (catch 'found
    (ledger-iterate-entries
     (function
      (lambda (start date mark desc)
	(if (ledger-time-less-p moment date)
	    (throw 'found t)))))))

(defun ledger-add-entry (entry-text)
  (interactive
   (list
    (read-string "Entry: " (concat ledger-year "/" ledger-month "/"))))
  (let* ((args (with-temp-buffer
		 (insert entry-text)
		 (eshell-parse-arguments (point-min) (point-max))))
	 (date (car args))
	 (insert-year t)
	 (ledger-buf (current-buffer))
	 exit-code)
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" date)
	(setq date
	      (encode-time 0 0 0 (string-to-number (match-string 3 date))
			   (string-to-number (match-string 2 date))
			   (string-to-number (match-string 1 date)))))
    (ledger-find-slot date)
    (save-excursion
      (if (re-search-backward "^Y " nil t)
	  (setq insert-year nil)))
    (save-excursion
      (insert
       (with-temp-buffer
	 (setq exit-code
	       (apply #'ledger-run-ledger ledger-buf "entry"
		      (mapcar 'eval args)))
	 (if (= 0 exit-code)
	     (if insert-year
		 (buffer-substring 2 (point-max))
	       (buffer-substring 7 (point-max)))
	   (concat (if insert-year entry-text
		     (substring entry-text 6)) "\n"))) "\n"))))

(defun ledger-current-entry-bounds ()
  (save-excursion
    (when (or (looking-at "^[0-9]")
	      (re-search-backward "^[0-9]" nil t))
      (let ((beg (point)))
	(while (not (eolp))
	  (forward-line))
	(cons (copy-marker beg) (point-marker))))))

(defun ledger-delete-current-entry ()
  (interactive)
  (let ((bounds (ledger-current-entry-bounds)))
    (delete-region (car bounds) (cdr bounds))))

(defun ledger-toggle-current-entry (&optional style)
  (interactive)
  (let (clear)
    (save-excursion
      (when (or (looking-at "^[0-9]")
		(re-search-backward "^[0-9]" nil t))
	(skip-chars-forward "0-9./=")
	(delete-horizontal-space)
	(if (member (char-after) '(?\* ?\!))
	    (progn
	      (delete-char 1)
	      (if (and style (eq style 'cleared))
		  (insert " *")))
	  (if (and style (eq style 'pending))
	      (insert " ! ")
	    (insert " * "))
	  (setq clear t))))
    clear))

(defun ledger-toggle-current-transaction (&optional style)
  "Toggle the cleared status of the transaction under point.
Optional argument STYLE may be `pending' or `cleared', depending
on which type of status the caller wishes to indicate (default is
`cleared').
This function is rather complicated because it must preserve both
the overall formatting of the ledger entry, as well as ensuring
that the most minimal display format is used.  This could be
achieved more certainly by passing the entry to ledger for
formatting, but doing so causes inline math expressions to be
dropped."
  (interactive)
  (let ((bounds (ledger-current-entry-bounds))
	clear cleared)
    ;; Uncompact the entry, to make it easier to toggle the
    ;; transaction
    (save-excursion
      (goto-char (car bounds))
      (skip-chars-forward "0-9./= \t")
      (setq cleared (and (member (char-after) '(?\* ?\!))
			 (char-after)))
      (when cleared
	(let ((here (point)))
	  (skip-chars-forward "*! ")
	  (let ((width (- (point) here)))
	    (when (> width 0)
	      (delete-region here (point))
	      (if (search-forward "  " (line-end-position) t)
		  (insert (make-string width ? ))))))
	(forward-line)
	(while (looking-at "[ \t]")
	  (skip-chars-forward " \t")
	  (insert cleared " ")
	  (if (search-forward "  " (line-end-position) t)
	      (delete-char 2))
	  (forward-line))))
    ;; Toggle the individual transaction
    (save-excursion
      (goto-char (line-beginning-position))
      (when (looking-at "[ \t]")
	(skip-chars-forward " \t")
	(let ((here (point))
	      (cleared (member (char-after) '(?\* ?\!))))
	  (skip-chars-forward "*! ")
	  (let ((width (- (point) here)))
	    (when (> width 0)
	      (delete-region here (point))
	      (save-excursion
		(if (search-forward "  " (line-end-position) t)
		    (insert (make-string width ? ))))))
	  (let (inserted)
	    (if cleared
		(if (and style (eq style 'cleared))
		    (progn
		      (insert "* ")
		      (setq inserted t)))
	      (if (and style (eq style 'pending))
		  (progn
		    (insert "! ")
		    (setq inserted t))
		(progn
		  (insert "* ")
		  (setq inserted t))))
	    (if (and inserted
		     (search-forward "  " (line-end-position) t))
		(delete-char 2))
	    (setq clear inserted)))))
    ;; Clean up the entry so that it displays minimally
    (save-excursion
      (goto-char (car bounds))
      (forward-line)
      (let ((first t)
	    (state ? )
	    (hetero nil))
	(while (and (not hetero) (looking-at "[ \t]"))
	  (skip-chars-forward " \t")
	  (let ((cleared (if (member (char-after) '(?\* ?\!))
			     (char-after)
			   ? )))
	    (if first
		(setq state cleared
		      first nil)
	      (if (/= state cleared)
		  (setq hetero t))))
	  (forward-line))
	(when (and (not hetero) (/= state ? ))
	  (goto-char (car bounds))
	  (forward-line)
	  (while (looking-at "[ \t]")
	    (skip-chars-forward " \t")
	    (let ((here (point)))
	      (skip-chars-forward "*! ")
	      (let ((width (- (point) here)))
		(when (> width 0)
		  (delete-region here (point))
		  (if (search-forward "  " (line-end-position) t)
		      (insert (make-string width ? ))))))
	    (forward-line))
	  (goto-char (car bounds))
	  (skip-chars-forward "0-9./= \t")
	  (insert state " ")
	  (if (search-forward "  " (line-end-position) t)
	      (delete-char 2)))))
    clear))

(defun ledger-toggle-current (&optional style)
  (interactive)
  (if ledger-clear-whole-entries
      (ledger-toggle-current-entry style)
    (ledger-toggle-current-transaction style)))

(defvar ledger-mode-abbrev-table)

;;;###autoload
(define-derived-mode ledger-mode text-mode "Ledger"
  "A mode for editing ledger data files."
  (set (make-local-variable 'comment-start) " ; ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-tabs-mode) nil)

  (if (boundp 'font-lock-defaults)
      (set (make-local-variable 'font-lock-defaults)
	   '(ledger-font-lock-keywords nil t)))

  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'ledger-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'ledger-complete-at-point)
  (set (make-local-variable 'pcomplete-termination-string) "")

  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?a)] 'ledger-add-entry)
    (define-key map [(control ?c) (control ?d)] 'ledger-delete-current-entry)
    (define-key map [(control ?c) (control ?y)] 'ledger-set-year)
    (define-key map [(control ?c) (control ?m)] 'ledger-set-month)
    (define-key map [(control ?c) (control ?c)] 'ledger-toggle-current)
    (define-key map [(control ?c) (control ?e)] 'ledger-toggle-current-entry)
    (define-key map [(control ?c) (control ?r)] 'ledger-reconcile)
    (define-key map [(control ?c) (control ?s)] 'ledger-sort)
    (define-key map [tab] 'pcomplete)
    (define-key map [(control ?i)] 'pcomplete)
    (define-key map [(control ?c) tab] 'ledger-fully-complete-entry)
    (define-key map [(control ?c) (control ?i)] 'ledger-fully-complete-entry)
    (define-key map [(control ?c) (control ?o) (control ?r)] 'ledger-report)
    (define-key map [(control ?c) (control ?o) (control ?g)] 'ledger-report-goto)
    (define-key map [(control ?c) (control ?o) (control ?a)] 'ledger-report-redo)
    (define-key map [(control ?c) (control ?o) (control ?s)] 'ledger-report-save)
    (define-key map [(control ?c) (control ?o) (control ?e)] 'ledger-report-edit)
    (define-key map [(control ?c) (control ?o) (control ?k)] 'ledger-report-kill)))

;; Reconcile mode

(defvar ledger-buf nil)
(defvar ledger-acct nil)

(defun ledger-display-balance ()
  (let ((buffer ledger-buf)
	(account ledger-acct))
    (with-temp-buffer
      (let ((exit-code (ledger-run-ledger buffer "-C" "balance" account)))
	(if (/= 0 exit-code)
	    (message "Error determining cleared balance")
	  (goto-char (1- (point-max)))
	  (goto-char (line-beginning-position))
	  (delete-horizontal-space)
	  (message "Cleared balance = %s"
		   (buffer-substring-no-properties (point)
						   (line-end-position))))))))

(defun ledger-reconcile-toggle ()
  (interactive)
  (let ((where (get-text-property (point) 'where))
	(account ledger-acct)
	(inhibit-read-only t)
	cleared)
    (when (equal (car where) "<stdin>")
      (with-current-buffer ledger-buf
	  (goto-char (cdr where))
	(setq cleared (ledger-toggle-current 'pending)))
      (if cleared
	  (add-text-properties (line-beginning-position)
			       (line-end-position)
			       (list 'face 'bold))
	(remove-text-properties (line-beginning-position)
				(line-end-position)
				(list 'face))))
    (forward-line)))

(defun ledger-auto-reconcile (balance date)
  (interactive "sReconcile to balance (negative for a liability): \nsStatement date (default: now): ")
  (let ((buffer ledger-buf)
	(account ledger-acct) cleared)
    ;; attempt to auto-reconcile in the background
    (with-temp-buffer
      (let ((exit-code
	     (ledger-run-ledger buffer "--format" "%xB\\n"
	      "--reconcile" balance "--reconcile-date" date
	      "register" account)))
	(if (/= 0 exit-code)
	    (error "Failed to reconcile account '%s' to balance '%s'"
		   account balance)
	  (goto-char (point-min))
	  (unless (looking-at "[0-9]")
	    (error (buffer-string)))
	  (while (not (eobp))
	    (setq cleared
		  (cons (1+ (read (current-buffer))) cleared))
	    (forward-line)))))
    (goto-char (point-min))
    (with-current-buffer ledger-buf
      (setq cleared (mapcar 'copy-marker (nreverse cleared))))
    (let ((inhibit-redisplay t))
      (dolist (pos cleared)
	(while (and (not (eobp))
		    (/= pos (cdr (get-text-property (point) 'where))))
	  (forward-line))
	(unless (eobp)
	  (ledger-reconcile-toggle))))
    (goto-char (point-min))))

(defun ledger-reconcile-refresh ()
  (interactive)
  (let ((inhibit-read-only t)
	(line (count-lines (point-min) (point))))
    (erase-buffer)
    (ledger-do-reconcile)
    (set-buffer-modified-p t)
    (goto-char (point-min))
    (forward-line line)))

(defun ledger-reconcile-refresh-after-save ()
  (let ((buf (get-buffer "*Reconcile*")))
    (if buf
	(with-current-buffer buf
	  (ledger-reconcile-refresh)
	  (set-buffer-modified-p nil)))))

(defun ledger-reconcile-add ()
  (interactive)
  (with-current-buffer ledger-buf
    (call-interactively #'ledger-add-entry))
  (ledger-reconcile-refresh))

(defun ledger-reconcile-delete ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (equal (car where) "<stdin>")
      (with-current-buffer ledger-buf
	(goto-char (cdr where))
	(ledger-delete-current-entry))
      (let ((inhibit-read-only t))
	(goto-char (line-beginning-position))
	(delete-region (point) (1+ (line-end-position)))
	(set-buffer-modified-p t)))))

(defun ledger-reconcile-visit ()
  (interactive)
  (let ((where (get-text-property (point) 'where)))
    (when (equal (car where) "<stdin>")
      (switch-to-buffer-other-window ledger-buf)
      (goto-char (cdr where)))))

(defun ledger-reconcile-save ()
  (interactive)
  (with-current-buffer ledger-buf
    (save-buffer))
  (set-buffer-modified-p nil)
  (ledger-display-balance))

(defun ledger-reconcile-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun ledger-reconcile-finish ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((where (get-text-property (point) 'where))
	    (face  (get-text-property (point) 'face)))
	(if (and (eq face 'bold)
		 (equal (car where) "<stdin>"))
	    (with-current-buffer ledger-buf
	      (goto-char (cdr where))
	      (ledger-toggle-current 'cleared))))
      (forward-line 1)))
  (ledger-reconcile-save))

(defun ledger-do-reconcile ()
  (let* ((buf ledger-buf)
	 (account ledger-acct)
	 (items
	  (with-temp-buffer
	    (let ((exit-code
		   (ledger-run-ledger buf "--uncleared" "emacs" account)))
	      (when (= 0 exit-code)
		(goto-char (point-min))
		(unless (eobp)
		  (unless (looking-at "(")
		    (error (buffer-string)))
		  (read (current-buffer))))))))
    (dolist (item items)
      (let ((index 1))
	(dolist (xact (nthcdr 5 item))
	  (let ((beg (point))
		(where
		 (with-current-buffer buf
		   (cons
		    (nth 0 item)
		    (if ledger-clear-whole-entries
			(copy-marker (nth 1 item))
		      (copy-marker (nth 0 xact)))))))
	    (insert (format "%s %-30s %-25s %15s\n"
			    (format-time-string "%m/%d" (nth 2 item))
			    (nth 4 item) (nth 1 xact) (nth 2 xact)))
	    (if (nth 3 xact)
		(set-text-properties beg (1- (point))
				     (list 'face 'bold
					   'where where))
	      (set-text-properties beg (1- (point))
				   (list 'where where))))
	  (setq index (1+ index)))))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only t)))

(defun ledger-reconcile (account &optional arg)
  (interactive "sAccount to reconcile: \nP")
  (let ((buf (current-buffer))
	(rbuf (get-buffer "*Reconcile*")))
    (if rbuf
	(kill-buffer rbuf))
    (add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save)
    (with-current-buffer
	(pop-to-buffer (get-buffer-create "*Reconcile*"))
      (ledger-reconcile-mode)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-acct) account)
      (ledger-do-reconcile)
      (when arg
	(sit-for 0 0)
	(call-interactively #'ledger-auto-reconcile)))))

(defvar ledger-reconcile-mode-abbrev-table)

(define-derived-mode ledger-reconcile-mode text-mode "Reconcile"
  "A mode for reconciling ledger entries."
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?m)] 'ledger-reconcile-visit)
    (define-key map [return] 'ledger-reconcile-visit)
    (define-key map [(control ?c) (control ?c)] 'ledger-reconcile-finish)
    (define-key map [(control ?c) (control ?r)] 'ledger-auto-reconcile)
    (define-key map [(control ?x) (control ?s)] 'ledger-reconcile-save)
    (define-key map [(control ?l)] 'ledger-reconcile-refresh)
    (define-key map [? ] 'ledger-reconcile-toggle)
    (define-key map [?a] 'ledger-reconcile-add)
    (define-key map [?d] 'ledger-reconcile-delete)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line)
    (define-key map [?r] 'ledger-auto-reconcile)
    (define-key map [?s] 'ledger-reconcile-save)
    (define-key map [?q] 'ledger-reconcile-quit)
    (use-local-map map)))

;; Context sensitivity

(defconst ledger-line-config
  '((entry
     (("^\\(\\([0-9][0-9][0-9][0-9]/\\)?[01]?[0-9]/[0123]?[0-9]\\)[ \t]+\\(\\([!*]\\)[ \t]\\)?[ \t]*\\((\\(.*\\))\\)?[ \t]*\\(.*?\\)[ \t]*;\\(.*\\)[ \t]*$"
       (date nil status nil nil code payee comment))
      ("^\\(\\([0-9][0-9][0-9][0-9]/\\)?[01]?[0-9]/[0123]?[0-9]\\)[ \t]+\\(\\([!*]\\)[ \t]\\)?[ \t]*\\((\\(.*\\))\\)?[ \t]*\\(.*\\)[ \t]*$"
       (date nil status nil nil code payee))))
    (acct-transaction
     (("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\([$]\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account commodity amount nil comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\([$]\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)[ \t]*$"
       (indent account commodity amount nil))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?[0-9]+\\(\\.[0-9]*\\)?\\)[ \t]+\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?[0-9]+\\(\\.[0-9]*\\)?\\)[ \t]+\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?\\(\\.[0-9]*\\)\\)[ \t]+\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]+\\(-?\\(\\.[0-9]*\\)\\)[ \t]+\\(.*?\\)[ \t]*$"
       (indent account amount nil commodity))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent account comment))
      ("\\(^[ \t]+\\)\\(.*?\\)[ \t]*$"
       (indent account))))))

(defun ledger-extract-context-info (line-type pos)
  "Get context info for current line.

Assumes point is at beginning of line, and the pos argument specifies
where the \"users\" point was."
  (let ((linfo (assoc line-type ledger-line-config))
	found field fields)
    (dolist (re-info (nth 1 linfo))
      (let ((re (nth 0 re-info))
	    (names (nth 1 re-info)))
	(unless found
	  (when (looking-at re)
	    (setq found t)
	    (dotimes (i (length names))
	      (when (nth i names)
		(setq fields (append fields
				     (list
				      (list (nth i names)
					    (match-string-no-properties (1+ i))
					    (match-beginning (1+ i))))))))
	    (dolist (f fields)
	      (and (nth 1 f)
		   (>= pos (nth 2 f))
		   (setq field (nth 0 f))))))))
    (list line-type field fields)))

(defun ledger-context-at-point ()
  "Return a list describing the context around point.

The contents of the list are the line type, the name of the field
point containing point, and for selected line types, the content of
the fields in the line in a association list."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (let ((first-char (char-after)))
	(cond ((equal (point) (line-end-position))
	       '(empty-line nil nil))
	      ((memq first-char '(?\ ?\t))
	       (ledger-extract-context-info 'acct-transaction pos))
	      ((memq first-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	       (ledger-extract-context-info 'entry pos))
	      ((equal first-char ?\=)
	       '(automated-entry nil nil))
	      ((equal first-char ?\~)
	       '(period-entry nil nil))
	      ((equal first-char ?\!)
	       '(command-directive))
	      ((equal first-char ?\;)
	       '(comment nil nil))
	      ((equal first-char ?Y)
	       '(default-year nil nil))
	      ((equal first-char ?P)
	       '(commodity-price nil nil))
	      ((equal first-char ?N)
	       '(price-ignored-commodity nil nil))
	      ((equal first-char ?D)
	       '(default-commodity nil nil))
	      ((equal first-char ?C)
	       '(commodity-conversion nil nil))
	      ((equal first-char ?i)
	       '(timeclock-i nil nil))
	      ((equal first-char ?o)
	       '(timeclock-o nil nil))
	      ((equal first-char ?b)
	       '(timeclock-b nil nil))
	      ((equal first-char ?h)
	       '(timeclock-h  nil nil))
	      (t
	       '(unknown nil nil)))))))

(defun ledger-context-other-line (offset)
  "Return a list describing context of line offset for existing position.

Offset can be positive or negative.  If run out of buffer before reaching
specified line, returns nil."
  (save-excursion
    (let ((left (forward-line offset)))
      (if (not (equal left 0))
	  nil
	(ledger-context-at-point)))))

(defun ledger-context-line-type (context-info)
  (nth 0 context-info))

(defun ledger-context-current-field (context-info)
  (nth 1 context-info))

(defun ledger-context-field-info (context-info field-name)
  (assoc field-name (nth 2 context-info)))

(defun ledger-context-field-present-p (context-info field-name)
  (not (null (ledger-context-field-info context-info field-name))))

(defun ledger-context-field-value (context-info field-name)
  (nth 1 (ledger-context-field-info context-info field-name)))

(defun ledger-context-field-position (context-info field-name)
  (nth 2 (ledger-context-field-info context-info field-name)))

(defun ledger-context-field-end-position (context-info field-name)
  (+ (ledger-context-field-position context-info field-name)
     (length (ledger-context-field-value context-info field-name))))

(defun ledger-context-goto-field-start (context-info field-name)
  (goto-char (ledger-context-field-position context-info field-name)))

(defun ledger-context-goto-field-end (context-info field-name)
  (goto-char (ledger-context-field-end-position context-info field-name)))

(defun ledger-entry-payee ()
  "Returns the payee of the entry containing point or nil."
  (let ((i 0))
    (while (eq (ledger-context-line-type (ledger-context-other-line i)) 'acct-transaction)
      (setq i (- i 1)))
    (let ((context-info (ledger-context-other-line i)))
      (if (eq (ledger-context-line-type context-info) 'entry)
	  (ledger-context-field-value context-info 'payee)
	nil))))

;; Ledger report mode

(defvar ledger-report-buffer-name "*Ledger Report*")

(defvar ledger-report-name nil)
(defvar ledger-report-cmd nil)
(defvar ledger-report-name-prompt-history nil)
(defvar ledger-report-cmd-prompt-history nil)
(defvar ledger-original-window-cfg nil)

(defvar ledger-report-mode-abbrev-table)

(define-derived-mode ledger-report-mode text-mode "Ledger-Report"
  "A mode for viewing ledger reports."
  (let ((map (make-sparse-keymap)))
    (define-key map [? ] 'scroll-up)
    (define-key map [?r] 'ledger-report-redo)
    (define-key map [?s] 'ledger-report-save)
    (define-key map [?k] 'ledger-report-kill)
    (define-key map [?e] 'ledger-report-edit)
    (define-key map [?q] 'ledger-report-quit)
    (define-key map [(control ?c) (control ?l) (control ?r)]
      'ledger-report-redo)
    (define-key map [(control ?c) (control ?l) (control ?S)]
      'ledger-report-save)
    (define-key map [(control ?c) (control ?l) (control ?k)]
      'ledger-report-kill)
    (define-key map [(control ?c) (control ?l) (control ?e)]
      'ledger-report-edit)
    (use-local-map map)))

(defun ledger-report-read-name ()
  "Read the name of a ledger report to use, with completion.

The empty string and unknown names are allowed."
  (completing-read "Report name: "
		   ledger-reports nil nil nil
		   'ledger-report-name-prompt-history nil))

(defun ledger-report (report-name edit)
  "Run a user-specified report from `ledger-reports'.

Prompts the user for the name of the report to run.  If no name is
entered, the user will be prompted for a command line to run.  The
command line specified or associated with the selected report name
is run and the output is made available in another buffer for viewing.
If a prefix argument is given and the user selects a valid report
name, the user is prompted with the corresponding command line for
editing before the command is run.

The output buffer will be in `ledger-report-mode', which defines
commands for saving a new named report based on the command line
used to generate the buffer, navigating the buffer, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
		(y-or-n-p "Buffer modified, save it? "))
       (save-buffer))
     (let ((rname (ledger-report-read-name))
	   (edit (not (null current-prefix-arg))))
       (list rname edit))))
  (let ((buf (current-buffer))
	(rbuf (get-buffer ledger-report-buffer-name))
	(wcfg (current-window-configuration)))
    (if rbuf
	(kill-buffer rbuf))
    (with-current-buffer
	(pop-to-buffer (get-buffer-create ledger-report-buffer-name))
      (ledger-report-mode)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-report-name) report-name)
      (set (make-local-variable 'ledger-original-window-cfg) wcfg)
      (ledger-do-report (ledger-report-cmd report-name edit))
      (shrink-window-if-larger-than-buffer))))

(defun string-empty-p (s)
  "Check for the empty string."
  (string-equal "" s))

(defun ledger-report-name-exists (name)
  "Check to see if the given report name exists.

If name exists, returns the object naming the report, otherwise returns nil."
  (unless (string-empty-p name)
    (car (assoc name ledger-reports))))

(defun ledger-reports-add (name cmd)
  "Add a new report to `ledger-reports'."
  (setq ledger-reports (cons (list name cmd) ledger-reports)))

(defun ledger-reports-custom-save ()
  "Save the `ledger-reports' variable using the customize framework."
  (customize-save-variable 'ledger-reports ledger-reports))

(defun ledger-report-read-command (report-cmd)
  "Read the command line to create a report."
  (read-from-minibuffer "Report command line: "
			(if (null report-cmd) "ledger " report-cmd)
			nil nil 'ledger-report-cmd-prompt-history))

(defun ledger-report-ledger-file-format-specifier ()
  "Substitute the full path to master or current ledger file

The master file name is determined by the ledger-master-file buffer-local
variable which can be set using file variables.  If it is set, it is used,
otherwise the current buffer file is used."
  (ledger-master-file))

(defun ledger-read-string-with-default (prompt default)
  (let ((default-prompt (concat prompt
				(if default
				    (concat " (" default "): ")
				  ": "))))
    (read-string default-prompt nil nil default)))

(defun ledger-report-payee-format-specifier ()
  "Substitute a payee name

The user is prompted to enter a payee and that is substitued.  If
point is in an entry, the payee for that entry is used as the
default."
  ;; It is intended copmletion should be available on existing
  ;; payees, but the list of possible completions needs to be
  ;; developed to allow this.
  (ledger-read-string-with-default "Payee" (regexp-quote (ledger-entry-payee))))

(defun ledger-report-account-format-specifier ()
  "Substitute an account name

The user is prompted to enter an account name, which can be any
regular expression identifying an account.  If point is on an account
transaction line for an entry, the full account name on that line is
the default."
  ;; It is intended completion should be available on existing account
  ;; names, but it remains to be implemented.
  (let* ((context (ledger-context-at-point))
	 (default
	  (if (eq (ledger-context-line-type context) 'acct-transaction)
	      (regexp-quote (ledger-context-field-value context 'account))
	    nil)))
    (ledger-read-string-with-default "Account" default)))

(defun ledger-report-expand-format-specifiers (report-cmd)
  (let ((expanded-cmd report-cmd))
    (while (string-match "%(\\([^)]*\\))" expanded-cmd)
      (let* ((specifier (match-string 1 expanded-cmd))
	     (f (cdr (assoc specifier ledger-report-format-specifiers))))
	(if f
	    (setq expanded-cmd (replace-match
				(save-match-data
				  (with-current-buffer ledger-buf
				    (shell-quote-argument (funcall f))))
				t t expanded-cmd))
	  (progn
	    (set-window-configuration ledger-original-window-cfg)
	    (error "Invalid ledger report format specifier '%s'" specifier)))))
    expanded-cmd))

(defun ledger-report-cmd (report-name edit)
  "Get the command line to run the report."
  (let ((report-cmd (car (cdr (assoc report-name ledger-reports)))))
    ;; logic for substitution goes here
    (when (or (null report-cmd) edit)
      (setq report-cmd (ledger-report-read-command report-cmd)))
    (setq report-cmd (ledger-report-expand-format-specifiers report-cmd))
    (set (make-local-variable 'ledger-report-cmd) report-cmd)
    (or (string-empty-p report-name)
	(ledger-report-name-exists report-name)
	(ledger-reports-add report-name report-cmd)
	(ledger-reports-custom-save))
    report-cmd))

(defun ledger-do-report (cmd)
  "Run a report command line."
  (goto-char (point-min))
  (insert (format "Report: %s\n" cmd)
	  (make-string (- (window-width) 1) ?=)
	  "\n")
  (shell-command cmd t nil))

(defun ledger-report-goto ()
  "Goto the ledger report buffer."
  (interactive)
  (let ((rbuf (get-buffer ledger-report-buffer-name)))
    (if (not rbuf)
	(error "There is no ledger report buffer"))
    (pop-to-buffer rbuf)
    (shrink-window-if-larger-than-buffer)))

(defun ledger-report-redo ()
  "Redo the report in the current ledger report buffer."
  (interactive)
  (ledger-report-goto)
  (erase-buffer)
  (ledger-do-report ledger-report-cmd))

(defun ledger-report-quit ()
  "Quit the ledger report buffer."
  (interactive)
  (ledger-report-goto)
  (set-window-configuration ledger-original-window-cfg))

(defun ledger-report-kill ()
  "Kill the ledger report buffer."
  (interactive)
  (ledger-report-quit)
  (kill-buffer (get-buffer ledger-report-buffer-name)))

(defun ledger-report-edit ()
  "Edit the defined ledger reports."
  (interactive)
  (customize-variable 'ledger-reports))

(defun ledger-report-read-new-name ()
  "Read the name for a new report from the minibuffer."
  (let ((name ""))
    (while (string-empty-p name)
      (setq name (read-from-minibuffer "Report name: " nil nil nil
				       'ledger-report-name-prompt-history)))
    name))

(defun ledger-report-save ()
  "Save the current report command line as a named report."
  (interactive)
  (ledger-report-goto)
  (let (existing-name)
    (when (string-empty-p ledger-report-name)
      (setq ledger-report-name (ledger-report-read-new-name)))

    (while (setq existing-name (ledger-report-name-exists ledger-report-name))
      (cond ((y-or-n-p (format "Overwrite existing report named '%s' "
			       ledger-report-name))
	     (when (string-equal
		    ledger-report-cmd
		    (car (cdr (assq existing-name ledger-reports))))
	       (error "Current command is identical to existing saved one"))
	     (setq ledger-reports
		   (assq-delete-all existing-name ledger-reports)))
	    (t
	     (setq ledger-report-name (ledger-report-read-new-name)))))

    (ledger-reports-add ledger-report-name ledger-report-cmd)
    (ledger-reports-custom-save)))

;; In-place completion support

(defun ledger-thing-at-point ()
  (let ((here (point)))
    (goto-char (line-beginning-position))
    (cond ((looking-at "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.+?)\\)?\\s-+")
	   (goto-char (match-end 0))
	   'entry)
	  ((looking-at "^\\s-+\\([*!]\\s-+\\)?[[(]?\\(.\\)")
	   (goto-char (match-beginning 2))
	   'transaction)
	  (t
	   (ignore (goto-char here))))))

(defun ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let* ((info (save-excursion
		 (cons (ledger-thing-at-point) (point))))
	 (begin (cdr info))
	 (end (point))
	 begins args)
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
	(skip-chars-forward " \t\n")
	(setq begins (cons (point) begins))
	(setq args (cons (buffer-substring-no-properties
			  (car begins) end)
			 args)))
      (cons (reverse args) (reverse begins)))))

(defun ledger-entries ()
  (let ((origin (point))
	entries-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
		      "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
	(unless (and (>= origin (match-beginning 0))
		     (< origin (match-end 0)))
	  (setq entries-list (cons (match-string-no-properties 3)
				   entries-list)))))
    (pcomplete-uniqify-list (nreverse entries-list))))

(defvar ledger-account-tree nil)

(defun ledger-find-accounts ()
  (let ((origin (point)) account-path elements)
    (save-excursion
      (setq ledger-account-tree (list t))
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)" nil t)
	(unless (and (>= origin (match-beginning 0))
		     (< origin (match-end 0)))
	  (setq account-path (match-string-no-properties 2))
	  (setq elements (split-string account-path ":"))
	  (let ((root ledger-account-tree))
	    (while elements
	      (let ((entry (assoc (car elements) root)))
		(if entry
		    (setq root (cdr entry))
		  (setq entry (cons (car elements) (list t)))
		  (nconc root (list entry))
		  (setq root (cdr entry))))
	      (setq elements (cdr elements)))))))))

(defun ledger-accounts ()
  (ledger-find-accounts)
  (let* ((current (caar (ledger-parse-arguments)))
	 (elements (and current (split-string current ":")))
	 (root ledger-account-tree)
	 (prefix nil))
    (while (cdr elements)
      (let ((entry (assoc (car elements) root)))
	(if entry
	    (setq prefix (concat prefix (and prefix ":")
				 (car elements))
		  root (cdr entry))
	  (setq root nil elements nil)))
      (setq elements (cdr elements)))
    (and root
	 (sort
	  (mapcar (function
		   (lambda (x)
		     (let ((term (if prefix
				     (concat prefix ":" (car x))
				   (car x))))
		       (if (> (length (cdr x)) 1)
			   (concat term ":")
			 term))))
		  (cdr root))
	  'string-lessp))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (while (pcomplete-here
	  (if (eq (save-excursion
		    (ledger-thing-at-point)) 'entry)
	      (ledger-entries)
	    (ledger-accounts)))))

(defun ledger-fully-complete-entry ()
  "Do appropriate completion for the thing at point"
  (interactive)
  (let ((name (caar (ledger-parse-arguments)))
	xacts)
    (save-excursion
      (when (eq 'entry (ledger-thing-at-point))
	(when (re-search-backward
	       (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
		       (regexp-quote name) "\\(\t\\|\n\\| [ \t]\\)") nil t)
	  (forward-line)
	  (while (looking-at "^\\s-+")
	    (setq xacts (cons (buffer-substring-no-properties
			       (line-beginning-position)
			       (line-end-position))
			      xacts))
	    (forward-line))
	  (setq xacts (nreverse xacts)))))
    (when xacts
      (save-excursion
	(insert ?\n)
	(while xacts
	  (insert (car xacts) ?\n)
	  (setq xacts (cdr xacts))))
      (forward-line)
      (goto-char (line-end-position))
      (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
	  (goto-char (match-end 0))))))

;; A sample function for $ users

(defun ledger-align-dollars (&optional column)
  (interactive "p")
  (if (= column 1)
      (setq column 48))
  (while (search-forward "$" nil t)
    (backward-char)
    (let ((col (current-column))
	  (beg (point))
	  target-col len)
      (skip-chars-forward "-$0-9,.")
      (setq len (- (point) beg))
      (setq target-col (- column len))
      (if (< col target-col)
	  (progn
	    (goto-char beg)
	    (insert (make-string (- target-col col) ? )))
	(move-to-column target-col)
	(if (looking-back "  ")
	    (delete-char (- col target-col))
	  (skip-chars-forward "^ \t")
	  (delete-horizontal-space)
	  (insert "  ")))
      (forward-line))))

(defun ledger-next-amount (&optional end)
  (when (re-search-forward "\\(  \\|\t\\| \t\\)[ \t]*-?\\([A-Z$]+ *\\)?\\(-?[0-9,]+?\\)\\(.[0-9]+\\)?\\( *[A-Z$]+\\)?\\([ \t]*@@?[^\n;]+?\\)?\\([ \t]+;.+?\\)?$" (marker-position end) t)
    (goto-char (match-beginning 0))
    (skip-syntax-forward " ")
    (- (or (match-end 4)
	   (match-end 3)) (point))))

(defun ledger-align-amounts (&optional column)
  "Align amounts in the current region.
This is done so that the last digit falls in COLUMN, which defaults to 48."
  (interactive "p")
  (if (= column 1)
      (setq column 48))
  (save-excursion
    (let* ((mark-first (< (mark) (point)))
	   (begin (if mark-first (mark) (point)))
	   (end (if mark-first (point-marker) (mark-marker)))
	   offset)
      (goto-char begin)
      (while (setq offset (ledger-next-amount end))
	(let ((col (current-column))
	      (target-col (- column offset))
	      adjust)
	  (setq adjust (- target-col col))
	  (if (< col target-col)
	      (insert (make-string (- target-col col) ? ))
	    (move-to-column target-col)
	    (if (looking-back "  ")
		(delete-char (- col target-col))
	      (skip-chars-forward "^ \t")
	      (delete-horizontal-space)
	      (insert "  ")))
	  (forward-line))))))

;; A sample entry sorting function, which works if entry dates are of
;; the form YYYY/mm/dd.

(defun ledger-sort ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (sort-subr
     nil
     (function
      (lambda ()
	(if (re-search-forward
	     (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+"
		     "\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)") nil t)
	    (goto-char (match-beginning 0))
	  (goto-char (point-max)))))
     (function
      (lambda ()
	(forward-paragraph))))))

;; General helper functions

(defvar ledger-delete-after nil)

(defun ledger-run-ledger (buffer &rest args)
  "run ledger with supplied arguments"
  (cond
   ((null ledger-binary-path)
    (error "The variable `ledger-binary-path' has not been set"))
   ((not (file-exists-p ledger-binary-path))
    (error "The file `ledger-binary-path' (\"%s\") does not exist"
	   ledger-binary-path))
   ((not (file-executable-p ledger-binary-path))
    (error "The file `ledger-binary-path' (\"%s\") cannot be executed"
	   ledger-binary-path))
   (t
    (let ((buf (current-buffer)))
      (with-current-buffer buffer
	(apply #'call-process-region
	       (append (list (point-min) (point-max)
			     ledger-binary-path ledger-delete-after
			     buf nil "-f" "-")
		       args)))))))

(defun ledger-run-ledger-and-delete (buffer &rest args)
  (let ((ledger-delete-after t))
    (apply #'ledger-run-ledger buffer args)))

(defun ledger-set-year (newyear)
  "Set ledger's idea of the current year to the prefix argument."
  (interactive "p")
  (if (= newyear 1)
      (setq ledger-year (read-string "Year: " (ledger-current-year)))
    (setq ledger-year (number-to-string newyear))))

(defun ledger-set-month (newmonth)
  "Set ledger's idea of the current month to the prefix argument."
  (interactive "p")
  (if (= newmonth 1)
      (setq ledger-month (read-string "Month: " (ledger-current-month)))
    (setq ledger-month (format "%02d" newmonth))))

(defvar ledger-master-file nil)

(defun ledger-master-file ()
  "Return the master file for a ledger file.

The master file is either the file for the current ledger buffer or the
file specified by the buffer-local variable ledger-master-file.  Typically
this variable would be set in a file local variable comment block at the
end of a ledger file which is included in some other file."
  (if ledger-master-file
      (expand-file-name ledger-master-file)
    (buffer-file-name)))

(provide 'ledger)

;;; ledger.el ends here
