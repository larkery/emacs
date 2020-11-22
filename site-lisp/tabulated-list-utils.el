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

(defun tabulated-list-marked-or-current-id (mark)
  (let (out)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (= mark (char-after))
          (push (tabulated-list-get-id) out))
        (forward-line)))
    (unless out
      (push (tabulated-list-get-id) out))
    out))

(defun tabulated-list-goto-id (id)
  (goto-char (point-min))
  (while (not (or (eobp)
                  (string= (tabulated-list-get-id) id)))
    (forward-line))
  (string= (tabulated-list-get-id) id))

(provide 'tabulated-list-utils)
