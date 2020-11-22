(require 'org)

(org-link-set-parameters
 "nm"
 :follow #'org-notmuch-link-follow
 :export #'org-notmuch-link-export
 :store  #'org-notmuch-link-store)

(defun org-notmuch-link-follow (query)
  ;; if query is for a message-id find the thread and go in it
  (if (string-match-p (rx bos (? "m") "id:") query)
      (let ((threads (notmuch-call-notmuch-sexp
                      "search" "--format=sexp" query)))
        (if (= 1 (length threads))
            (notmuch-show (plist-get (car threads) :thread)
                          nil nil query)
          (notmuch-search query)))
    (notmuch-search query)))

(defun org-notmuch-link-export (link desc fmt)
  (format "Email: %s" desc))

(defun org-notmuch-link-store ()
  (when (memq major-mode '(notmuch-show-mode))
    (org-store-link-props
     :type "nm"
     :link (format "nm:%s" (notmuch-show-get-message-id))
     :description (format "✉ %s" (notmuch-show-get-subject)))))

(provide 'org-notmuch-link)
