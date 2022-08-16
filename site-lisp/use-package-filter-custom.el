(require 'pp)
(require 'cl-lib)

(defvar use-package-customised-variables (make-hash-table))

(defun use-package-remember-custom (name _keyword args)
  (let ((file-and-package (cons (or load-file-name
                                    buffer-file-name)
                                name)))
    (cl-loop
     for cell in args
     do (puthash (car cell) file-and-package
                 use-package-customised-variables))))

(advice-add 'use-package-normalize/:custom :after 'use-package-remember-custom)

(defun use-package-filter-custom-format-custom (symbol)
  (princ "(")
  (prin1 symbol)
  (princ " ")
  
  ;; default-value here because use-package does setq-default
  ;; not set-custom
  (let* ((value (default-value symbol))
         (val (prin1-to-string value)))
    (unless (or (numberp value)
                (stringp value)
                (keywordp value)
                (vectorp value)
                (eq t value)
                (eq nil value))
      (princ "'"))
    (if (< (length val) 60)
	(insert val)
      (newline-and-indent)
      (let ((beginning-of-val (point)))
	(insert val)
	(save-excursion
	  (goto-char beginning-of-val)
	  (indent-pp-sexp 1)))))
  (princ ")"))

(defun use-package-filter-custom-file (&rest _)
  (goto-char (point-min))
  (search-forward "(custom-set-variables" nil t)
  (backward-up-list)
  (let ((customs (save-excursion (read (current-buffer)))))
    (kill-sexp)
    (let ((standard-output (current-buffer)))
      (princ "(custom-set-variables\n")
      (cl-loop
       for setting in (cdr customs)
       unless (gethash (caadr setting) use-package-customised-variables)
       do (pp setting))
      (princ ")\n")))
  (let ((inverse (make-hash-table :test #'equal)))
    (cl-loop for fp being the hash-values of use-package-customised-variables
             using (hash-keys symbol)
             do (push symbol (gethash fp inverse)))
    (cl-loop for fp being the hash-keys of inverse
             using (hash-values symbols)
             when (and (car fp)
                       (featurep (cdr fp))
                       (cl-every
                        (lambda (s) (or (fboundp s) (boundp s)))
                        symbols))
             do (let (kill-ring)
                  (with-current-buffer (find-file-noselect (car fp))
                    (save-mark-and-excursion
                      (goto-char (point-min))
                      (when (search-forward (format "(use-package %s" (cdr fp)) nil t)
                        (backward-up-list)
                        (mark-sexp)
                        (save-restriction
                          (narrow-to-region (point) (mark))
                          (goto-char (point-min))
                          (down-list)
                          (condition-case nil
                              (while (not (looking-at ":custom"))
                                (forward-sexp 2)
                                (backward-sexp))
                            (error nil))
                          (when (looking-at ":custom")
                            (forward-sexp 2)
                            (backward-sexp)
                            (let ((start-custom (point)))
                              (condition-case nil
                                  (while (looking-at "(")
                                    (forward-sexp 2)
                                    (backward-sexp))
                                (error nil))
                              (narrow-to-region start-custom (point)))

                            (dolist (sym symbols)
                              (goto-char (point-min))
                              (when (search-forward (format "(%s " sym)
                                                    nil t)
                                (backward-up-list)
                                (kill-sexp)
                                (let ((standard-output (current-buffer)))
                                  (use-package-filter-custom-format-custom sym)))
                              )
                            ))
                        ))
                    (save-buffer)
                    )))))

(advice-add 'custom-save-variables :after 'use-package-filter-custom-file)

(provide 'use-package-filter-custom)
