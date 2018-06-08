(use-package notmuch-agenda
  :defer t
  :commands notmuch-agenda-insert-part)

(use-package notmuch
  :commands notmuch
  :bind
  (("C-c m" . counsel-notmuch)
   :map notmuch-search-mode-map
	("f" . notmuch-search-flag)
	("d" . notmuch-search-delete)
	("g" . notmuch-refresh-this-buffer)
        :map notmuch-message-mode-map
        ("C-c f" . notmuch-switch-identity))
  
  :config

  (defvar counsel-notmuch-history nil)
  
  (defun counsel-notmuch ()
    (interactive)
    (let* ((search
            (ivy-read "Notmuch: "
                      (mapcar (lambda (x) (plist-get x :name)) notmuch-saved-searches)
                      :history 'counsel-notmuch-history
                      :caller 'counsel-notmuch-blah))
           (match (car (remove-if-not (lambda (x)
                                        (string= (plist-get x :name) search))
                                      notmuch-saved-searches))))
      (if match
          (notmuch-search (plist-get match :query))
        (notmuch-search search))))
  
  (require 'notmuch-switch-identity)
  (fset 'notmuch-show-insert-part-text/calendar #'notmuch-agenda-insert-part)

  (defun in-home-directory (o &rest args)
    (let ((default-directory "~"))
      (apply o args)))

  (advice-add 'notmuch :around 'in-home-directory)
  
  (setq notmuch-multipart/alternative-discouraged '("text/plain") ;; prefer html?

        notmuch-saved-searches
        '((:name "needs action" :query "tag:unread and tag:inbox" :key "j")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "inbox" :query "tag:inbox" :key "i")
          (:name "drafts" :query "tag:draft" :key "d"))

        notmuch-tag-formats
        '(("unread" (propertize tag 'face 'notmuch-tag-unread))
          ("flagged" (propertize tag 'face 'notmuch-tag-flagged)
           (notmuch-tag-format-image-data tag (notmuch-tag-star-icon)))
          ("low-importance" "↓")
          ("high-importance" "!")
          ("normal-importance")

          ("sent" "→")
          ("replied" "⮱")
          ("attachment" "📎")
          ("meeting" "📅")
          ("inbox" "I")
          ("accepted" "🗸")
          ("rejected" "❌")
          )
        notmuch-search-result-format
        '(("date" . "%12s  ")
          ;;("count" . "%-7s ")
          ("authors" . "%-20s ")
          ("subject" . "%s ")
          ("tags" . "%s"))
        
        notmuch-search-oldest-first nil
	notmuch-fcc-dirs
	'(("tom\\.hinton@cse\\.org\\.uk" . "\"cse/Sent Items\" +sent -inbox")
	  ("larkery\\.com" . "\"fastmail/Sent Items\" +sent -inbox"))
	notmuch-identities
	'("Tom Hinton <tom.hinton@cse.org.uk>" "Tom Hinton <t@larkery.com>")
        notmuch-draft-folders
        '(("tom\\.hinton@cse\\.org\\.uk" . "cse/Drafts")
	  ("larkery\\.com" . "fastmail/Drafts"))

        notmuch-address-selection-function
        (lambda
          (prompt collection initial-input)
          (completing-read prompt collection nil nil nil
                           (quote notmuch-address-history)))
	)

  (defun notmuch-search-buffer-title (query)
    "Returns the title for a buffer with notmuch search results."
    (let* ((saved-search
            (let (longest
                  (longest-length 0))
              (loop for tuple in notmuch-saved-searches
                    if (let ((quoted-query (regexp-quote (notmuch-saved-search-get tuple :query))))
                         (and (string-match (concat "^" quoted-query) query)
                              (> (length (match-string 0 query))
                                 longest-length)))
                    do (setq longest tuple
                             longest-length (length (match-string 0 query))))
              longest))
           (saved-search-name (notmuch-saved-search-get saved-search :name))
           (saved-search-query (notmuch-saved-search-get saved-search :query)))
      (cond ((and saved-search (equal saved-search-query query))
             ;; Query is the same as saved search (ignoring case)
             (concat "*notmuch-saved-search-" saved-search-name "*"))
            (saved-search
             (concat "*notmuch-search-"
                     (replace-regexp-in-string (concat "^" (regexp-quote saved-search-query))
                                               (concat "[ " saved-search-name " ]")
                                               query)
                     "*"))
            (t
             (concat "*notmuch-search-" query "*"))
            )))
  
  (defun notmuch-search-toggle-tag (&rest tags)
    (let* ((cur-tags (notmuch-search-get-tags))
	   (action (if (cl-intersection cur-tags tags :test 'string=) "-" "+"))
	   (arg (mapcar (lambda (x) (concat action x)) tags)))
      (notmuch-search-tag arg)))

  (defun notmuch-search-flag ()
    (interactive)
    (notmuch-search-toggle-tag "flagged"))
  
  (defun notmuch-search-delete ()
    (interactive)
    (notmuch-search-toggle-tag "deleted")
    (notmuch-search-next-thread))

  (defun notmuch-expand-calendar-parts (o msg part depth &optional hide)
    (funcall o
             msg part depth (and hide
                                 (not (string= (downcase (plist-get part :content-type))
                                               "text/calendar")))))
  
  (advice-add 'notmuch-show-insert-bodypart :around #'notmuch-expand-calendar-parts)

  (defun replace-notmuch-insert-part-text/html (msg part content-type nth depth button)
    (let* ((shr-blocked-images notmuch-show-text/html-blocked-images)
           (shr-width (- (window-width) 1 (* depth notmuch-show-indent-messages-width)))
           (start (if button (button-start button) (point)))
           (result (notmuch-show--insert-part-text/html-shr msg part))
           )
      (save-excursion
        (save-restriction
          (narrow-to-region start (point-max))
          (delete-trailing-whitespace (point-min) (point-max))
          (goto-char (point-min))
          (notmuch-wash-excerpt-citations msg depth)
          ))
      result))
  
  (advice-add 'notmuch-show-insert-part-text/html
              :override 'replace-notmuch-insert-part-text/html)

  (defun notmuch-show-skip-to-unread ()
    (interactive)
     (while (and (not (member "unread" (notmuch-show-get-tags)))
                 (notmuch-show-goto-message-next)))
     (notmuch-show-message-visible (notmuch-show-get-message-properties) t)
     (recenter-top-bottom 0))

  (bind-key "u" 'notmuch-show-skip-to-unread 'notmuch-show-mode-map)

;;  (add-hook 'notmuch-show-mode-hook 'notmuch-show-skip-to-unread)
  
  )


(use-package message
  :defer t
  :config

  (defvar message-signatures-alist
    `(( ,(rx "tom.hinton@cse.org.uk") .
        "Tom Hinton (📞 0117 934 1455)")))
  
  (defun message-signature-select-by-from ()
    (let ((from (save-restriction
                  (message-narrow-to-headers-or-head)
                  (mail-fetch-field "From")))
          result
          (choices message-signatures-alist))
      (while (and (not result) choices)
        (message "%s %s" (caar choices) (cdar choices))
        (when (string-match-p (caar choices) from)
          (setq result (cdar choices))
          (message "%s" result)
          (setq choices (cdr choices))))
      result))
  
  (setq
   message-send-mail-function 'message-send-mail-with-sendmail
   message-sendmail-envelope-from 'header
   mm-coding-system-priorities '(utf-8)
   mm-inline-large-images 'resize
   mm-inline-large-images-proportion 0.9
   mm-inline-text-html-with-images t
   sendmail-program "msmtpq-quiet"
   user-mail-address "tom.hinton@cse.org.uk"
   message-auto-save-directory nil
   message-interactive nil
   message-kill-buffer-on-exit t
   message-default-charset 'utf-8
   user-full-name "Tom Hinton"
   message-signature 'message-signature-select-by-from

   message-citation-line-function 'message-insert-formatted-citation-line

   message-yank-empty-prefix ""
   )
  )

(use-package mailcap
  :defer t
  :config
  (mailcap-add "application/pdf" 'pdf-view-mode)
  (mailcap-add "application/msword" 'pdf-view-word-document)
  (mailcap-add "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
               'pdf-view-word-document))


(use-package shr
  :defer t
  :config
  (setq shr-color-visible-luminance-min 75
        shr-use-fonts nil)

  (defun shr-colorise-region-ignore-bg
      (shr-colorise-region start end fg &optional bg)
    (funcall shr-colorise-region start end fg nil))
  
  (advice-add 'shr-colorize-region :around 'shr-colorise-region-ignore-bg)

  (defun disabling-gc (o &rest args)
    (let* (;; (start (current-time))

           (gc-cons-threshold (* 100 gc-cons-threshold))
           (result (apply o args))
           ;; (end (current-time))

           )
      ;; (message "%.2f" (float-time (subtract-time end start)))

      result))

  (advice-add 'shr-insert-document :around 'disabling-gc))

