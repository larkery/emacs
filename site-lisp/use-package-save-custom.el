;; this fails because autoloading does not require but does provide. I
;; could advise provide, but a feature can be in many files. it looks
;; like basically only regexes can work, and there is no easy way to
;; otherwise associate a custom variable with a package.

(require 'cl)

(defvar use-package-locations nil)

(defun use-package-record-location (sym &rest args)
  (push (cons sym load-file-name) use-package-locations))

(advice-add 'use-package :before 'use-package-record-location)

(defun use-package-format-custom-for (symbols &optional quoted)
  "Given a list of `SYMBOLS', output `custom-set-variables' stuff for them.
If `QUOTED' quote each thing, else don't"
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (dolist (symbol symbols)
        (let ((spec (car-safe (get symbol 'theme-value)))
	      (value (get symbol 'saved-value))
	      (requests (get symbol 'custom-requests))
	      (now (and (not (custom-variable-p symbol))
		        (or (boundp symbol)
			    (eq (get symbol 'force-value)
			        'rogue))))
	      (comment (get symbol 'saved-variable-comment)))
          ;; Check REQUESTS for validity.
          (dolist (request requests)
	    (when (and (symbolp request) (not (featurep request)))
	      (message "Unknown requested feature: %s" request)
	      (setq requests (delq request requests))))
          ;; Is there anything customized about this variable?
          (when (or (and spec (eq (car spec) 'user))
		    comment
		    (and (null spec) (get symbol 'saved-value)))
	    (unless (bolp)
	      (princ "\n"))
	    (if quoted
                (princ "  '(")
                (princ "  ("))
	    (prin1 symbol)
	    (princ " ")
	    (let ((val (prin1-to-string (car value))))
	      (if (< (length val) 60)
	          (insert val)
	        (newline-and-indent)
	        (let ((beginning-of-val (point)))
	          (insert val)
	          (save-excursion
		    (goto-char beginning-of-val)
		    (indent-pp-sexp 1)))))
	    (when (or now requests comment)
	      (princ " ")
	      (prin1 now)
	      (when (or requests comment)
	        (princ " ")
	        (prin1 requests)
	        (when comment
	          (princ " ")
	          (prin1 comment))))
	    (princ ")"))))
      (buffer-string))))

(defun use-package-save-custom-for (symbols)
  "Assuming the current buffer is narrowed to a use-package
statement, write a :custom for it."

  (let ((custom-block (use-package-format-custom-for symbols)))
    (if (search-forward-regexp ":custom\\(\\s-+\\|\n\\|(\\)" nil t)
        (let ((custom-start (point)))
          (while (not (or (eobp)
                          (looking-at
                           "\\()\\|\\(\\s-\\|\n\\)*:[a-z-]+\\)")))
            (forward-sexp))
          (delete-region custom-start (point)))
      (goto-char (point-min))
      (forward-sexp)
      (backward-char)
      (insert "\n:custom\n"))
    (insert custom-block))
  (indent-region (point-min) (point-max)))

(defun use-package-save-custom ()
  (interactive)
  (let ((package-symbols (make-list 0 1))
        (other-symbols (make-list 0 1))

        (prefix-match
         (rx-to-string `(: bos (|
                                ,@(cl-loop
                                   for p in use-package-locations
                                   collect (symbol-name (car p))))))))
    (mapatoms
     (lambda (symbol)
       (when (and (get symbol 'saved-value)
	          ;; ignore theme values
	          (or (null (get symbol 'theme-value))
		      (eq 'user (caar (get symbol 'theme-value)))))
         (let* ((sn (symbol-name symbol))
                (matches (string-match prefix-match sn)))
           (if matches
               (let* ((package (intern (match-string 0 sn)))
                      (this-package-symbols (alist-get package package-symbols)))
                 (if this-package-symbols
                     (push symbol (cdr this-package-symbols))
                   (push (list package symbol) package-symbols)))
             (push symbol other-symbols))))))
    
    ;; now we have symbols to save grouped by package they are sort of inside.
    
    (dolist (this-package-symbols package-symbols)
      (let* ((package (car this-package-symbols))
             (symbols (cdr this-package-symbols))
             (init-file (alist-get package use-package-locations)))
        (message "init file for %s: %s" package init-file)
        (when init-file
          (with-current-buffer (find-file-noselect init-file t)
            (when (search-forward-regexp
                   (concat "(use-package[[:blank:]]+" (regexp-quote (symbol-name package)))
                   nil t)
              (save-restriction
                (narrow-to-defun)
                (goto-char (point-min))
                (use-package-save-custom-for symbols)))))))
    
    (with-current-buffer (find-file-noselect custom-file t)
      (save-restriction
        (goto-char (point-min))
        (if (search-forward "(custom-set-variables")
            (progn (narrow-to-defun)
                   (delete-region (point-min) (point-max)))
          (goto-char (point-max)))
        (insert "(custom-set-variables\n")
        (insert (use-package-format-custom-for other-symbols t))
        (insert ")\n")))))

(provide 'use-package-save-custom)
