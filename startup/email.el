(use-package notmuch
  :commands notmuch
  :bind
  (:map notmuch-search-mode-map
	("f" . notmuch-search-flag)
	("d" . notmuch-search-delete)
	("g" . notmuch-refresh-this-buffer))
  
  :config
  (setq notmuch-search-oldest-first nil
	notmuch-fcc-dirs
	'(("tom\\.hinton@cse\\.org\\.uk" . "\"cse/Sent Items\" +sent -inbox")
	  ("larkery\\.com" . "\"fastmail/Sent Items\" +sent -inbox"))
	notmuch-identities
	'("Tom Hinton <tom.hinton@cse.org.uk>" "Tom Hinton <t@larkery.com>")
	)
  
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
  )

(use-package message
  :defer t
  :config
  (setq
   message-send-mail-function 'message-send-mail-with-sendmail
   message-sendmail-envelope-from 'header
   mm-coding-system-priorities '(utf-8)
   mm-inline-large-images 'resize
   mm-inline-large-images-proportion 0.5
   mm-inline-text-html-with-images t
   sendmail-program "msmtpq-quiet"
   user-mail-address "tom.hinton@cse.org.uk"))
