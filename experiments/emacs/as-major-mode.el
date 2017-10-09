diff -r 90472db4ef14 editors/emacs/beancount.el
--- a/editors/emacs/beancount.el	Sun Nov 20 14:18:59 2016 -0500
+++ b/editors/emacs/beancount.el	Sun Nov 20 14:58:24 2016 -0500
@@ -155,10 +155,11 @@
 (defun beancount--goto-bob () (goto-char (point-min)))

 ;;;###autoload
-(define-minor-mode beancount-mode
-  "A minor mode to help editing Beancount files.
-This can be used within other text modes, in particular, org-mode
-is great for sectioning large files with many transactions.
+(define-derived-mode beancount-mode special-mode "Beancount"
+  "A major mode to help editing Beancount files.
+
+org-mode style fold/outline capability can be added by turning on
+orgstruct-mode minor mode.

 \\{beancount-mode-map}"
   :init-value nil
@@ -200,15 +201,12 @@
   (set (make-local-variable 'font-lock-keywords-only) nil)
   ;; Important: you have to use 'nil for the mode here because in certain major
   ;; modes (e.g. org-mode) the font-lock-keywords is a buffer-local variable.
-  (if beancount-mode
-      (font-lock-add-keywords nil beancount-font-lock-keywords)
-    (font-lock-remove-keywords nil beancount-font-lock-keywords))
+  (font-lock-add-keywords nil beancount-font-lock-keywords)
   (if (fboundp 'font-lock-flush)
       (font-lock-flush)
     (with-no-warnings (font-lock-fontify-buffer)))

-  (when beancount-mode
-    (beancount-init-accounts))
+  (beancount-init-accounts)
   )

 (defvar beancount-accounts nil
