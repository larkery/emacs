(require 'notmuch)

(defvar notmuch-fancy-html-is-replying nil)

(defun notmuch-mua-reply-with-html-renderer (o &rest args)
  (let ((notmuch-fancy-html-is-replying t))
    (apply o args)))

(advice-add 'notmuch-mua-reply :around 'notmuch-mua-reply-with-html-renderer)

(defun notmuch-fancy-html-insert-part-text/html (msg part content-type nth depth button)
  (if notmuch-fancy-html-is-replying
      (when (string= content-type "text/html")
        (insert "#+BEGIN_QUOTE\n")
        ;; this leaves in invalid cid: links which we should kill
        (save-restriction
          (narrow-to-region (point) (point))
          (insert (notmuch-get-bodypart-text msg part t))
          (call-process-region
           (point-min) (point-max)
           "pandoc"
           t t
           nil
           "-f" "html" "-t" "org"))
        (insert "#+END_QUOTE\n")
        (goto-char (point-min))
        (replace-string "#+BEGIN_QUOTE" "#+HTML: <blockquote>")
        (goto-char (point-min))
        (replace-string "#+END_QUOTE" "#+HTML: </blockquote>")
        t)
    (let* ((shr-blocked-images notmuch-show-text/html-blocked-images)
           (shr-width (- (window-width) 1 (* depth notmuch-show-indent-messages-width)))
           (start (if button (button-start button) (point)))
           (result (notmuch-show--insert-part-text/html-shr msg part)))
      (save-excursion
        (save-restriction
          (narrow-to-region start (point-max))
          (delete-trailing-whitespace (point-min) (point-max))
          (goto-char (point-min))
          (notmuch-wash-excerpt-citations msg depth)))
      result)))
  
(advice-add 'notmuch-show-insert-part-text/html
            :override
            'notmuch-fancy-html-insert-part-text/html)

(defun notmuch-fancy-html-citation ()
  (let* ((should-cite (not (save-excursion
                             (search-forward-regexp (rx bol (| "#+BEGIN_QUOTE"
                                                               "#+HTML: <blockquote>")
                                                        eol)
                                                    nil t))))
         (message-yank-prefix
          (if should-cite message-yank-prefix ""))
         (message-yank-cited-prefix
          (if should-cite message-yank-cited-prefix ""))
         (message-yank-empty-prefix
          (if should-cite message-yank-empty-prefix ""))
         (message-indentation-spaces
          (if should-cite message-indentation-spaces 1)))
    (message-cite-original)))

(setq notmuch-mua-cite-function #'notmuch-fancy-html-citation)

(provide 'notmuch-fancy-html)
