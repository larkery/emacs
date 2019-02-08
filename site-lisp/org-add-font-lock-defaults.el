(defun org-add-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (let* ((em org-fontify-emphasized-text)
	 (lk org-highlight-links)
	 (org-font-lock-extra-keywords
	  (list
	   `(,(if org-fontify-whole-heading-line
		  "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
		"^\\(\\**\\)\\(\\* \\)\\(.*\\)")
	     (1 (org-get-level-face 1))
	     (2 (org-get-level-face 2))
	     (3 (org-get-level-face 3)))
	   ;; Table lines
	   '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
	     (1 'org-table t))
	   ;; Table internals
	   '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
	   '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
	   '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
	   '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
	   ;; Link related fontification.
	   '(org-activate-links)
	   (when (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
	   (when (memq 'radio lk) '(org-activate-target-links (1 'org-link t)))
	   (when (memq 'date lk) '(org-activate-dates (0 'org-date t)))
	   (when (memq 'footnote lk) '(org-activate-footnote-links))

	   ;; Emphasis
	   (when em '(org-do-emphasis-faces))
	   ;; Checkboxes
	   '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
	     1 'org-checkbox prepend)
	   (when (cdr (assq 'checkbox org-list-automatic-rules))
	     '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
	       (0 (org-get-checkbox-statistics-face) t)))
	   ;; Description list items
	   '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
	     1 'org-list-dt prepend)

	   ;; Specials
	   '(org-do-latex-and-related)
	   '(org-fontify-entities)
	   '(org-raise-scripts)
	   ;; Code
	   '(org-activate-code (1 'org-code t))
	   ;; COMMENT
	   (list (format
		  "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
		  org-todo-regexp
		  org-comment-string)
		 '(9 'org-special-keyword t))
	   ;; Blocks and meta lines
	   '(org-fontify-meta-lines-and-blocks))
          ))
    
    
    (setq org-font-lock-extra-keywords (delq nil org-font-lock-extra-keywords))
    ;; Now set the full font-lock-keywords
    (setq-local org-font-lock-keywords org-font-lock-extra-keywords)
    (font-lock-add-keywords nil org-font-lock-keywords)
    
    nil))
(provide 'org-add-font-lock-defaults)