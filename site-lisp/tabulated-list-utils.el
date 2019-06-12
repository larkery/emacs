(require 'tabulated-list)



(defun tabulated-list-put-tag-all (tag)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag tag t))))

(defun tabulated-list-put-tag-region (tag &optional next-line)
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (not (eobp))
            (tabulated-list-put-tag tag t))))
    (tabulated-list-put-tag tag next-line)))

(provide 'tabulated-list-utils)
