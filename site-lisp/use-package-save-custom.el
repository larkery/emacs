(defvar *current-feature* nil)
(defvar feature-by-custom-variable nil)

(defun require-record-current-feature (require feature &optional filename noerror)
  (let ((*current-feature* feature))
    (funcall require feature filename noerror )))

(advice-add 'require :around #'require-record-current-feature)

(defun defcustom-record-current-feature (symbol &rest args)
  (push (cons symbol *current-feature*) feature-by-custom-variable))

(advice-add 'custom-declare-variable :before #'defcustom-record-current-feature)

(defvar use-package-locations nil)

(defun use-package-record-location (sym &rest args)
  (push (cons sym load-file-name) use-package-locations))

(advice-add 'use-package :before 'use-package-record-location)

(defun use-package-save-custom ()
  (interactive)
  (let ((saved-list (make-list 0 1)))
    (mapatoms
     (lambda (symbol)
       (when (and (get symbol 'saved-value)
	          ;; ignore theme values
	          (or (null (get symbol 'theme-value))
		      (eq 'user (caar (get symbol 'theme-value)))))
         (let* ((symbol-feature (or (alist-get symbol feature-by-custom-variable)
                                    'custom))
                (feature-symbols (alist-get symbol-feature saved-list)))
           (if feature-symbols
               (push symbol (cdr feature-symbols))
             (push (list symbol-feature symbol) saved-list))))
       ))
    ;; now we have symbols to save grouped by feature they are sort of inside.
    
    (dolist (feature-symbols saved-list)
      (with-temp-buffer
        (let* ((standard-output (current-buffer))
               (feature (car feature-symbols))
               (symbols (cdr feature-symbols))
               (init-file (alist-get feature use-package-locations)))
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
	        ;; Output an element for this variable.
	        ;; It has the form (SYMBOL VALUE-FORM NOW REQUESTS COMMENT).
	        ;; SYMBOL is the variable name.
	        ;; VALUE-FORM is an expression to return the customized value.
	        ;; NOW if non-nil means always set the variable immediately
	        ;; when the customizations are reloaded.  This is used
	        ;; for rogue variables
	        ;; REQUESTS is a list of packages to load before setting the
	        ;; variable.  Each element of it will be passed to `require'.
	        ;; COMMENT is whatever comment the user has specified
	        ;; with the customize facility.
	        (unless (bolp)
	          (princ "\n"))
	        (princ "  (")
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

          (let ((custom-block (buffer-string)))
            (when init-file
              (with-current-buffer (find-file-noselect init-file t)
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward-regexp
                         (concat "(use-package[[:blank:]]+" (regexp-quote (symbol-name feature)))
                         nil t)
                    (save-restriction
                      (narrow-to-defun)
                      (goto-char (point-min))
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
                        (insert ":custom\n"))
                      (insert custom-block))))))))))))
                        
                        
                        


(provide 'use-package-save-custom)
