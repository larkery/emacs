(use-package notmuch-agenda
  :defer t
  :commands notmuch-agenda-insert-part)

(use-package notmuch
  :commands notmuch
  :bind
  (("C-c m" . notmuch)
   :map notmuch-search-mode-map
	("f" . notmuch-search-flag)
	("d" . notmuch-search-delete)
	("g" . notmuch-refresh-this-buffer)
        :map notmuch-message-mode-map
        ("C-c m" . notmuch-switch-identity))
  
  :config
  (require 'notmuch-switch-identity)
  (fset 'notmuch-show-insert-part-text/calendar #'notmuch-agenda-insert-part)

  (defun in-home-directory (o &rest args)
    (let ((default-directory "~"))
      (apply o args)))

  (advice-add 'notmuch :around 'in-home-directory)
  
  (setq notmuch-search-oldest-first nil
	notmuch-fcc-dirs
	'(("tom\\.hinton@cse\\.org\\.uk" . "\"cse/Sent Items\" +sent -inbox")
	  ("larkery\\.com" . "\"fastmail/Sent Items\" +sent -inbox"))
	notmuch-identities
	'("Tom Hinton <tom.hinton@cse.org.uk>" "Tom Hinton <t@larkery.com>")
        notmuch-draft-folders
        '(("tom\\.hinton@cse\\.org\\.uk" . "cse/Drafts")
	  ("larkery\\.com" . "fastmail/Drafts"))
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

  (defun notmuch-expand-calendar-parts (o msg part depth &optional hide)
    (funcall o
             msg part depth (and hide
                                 (not (string= (downcase (plist-get part :content-type))
                                               "text/calendar")))))
  
  (advice-add 'notmuch-show-insert-bodypart :around #'notmuch-expand-calendar-parts)
  )

(use-package message
  :defer t
  :config
  (setq
   message-send-mail-function 'message-send-mail-with-sendmail
   message-sendmail-envelope-from 'header
   mm-coding-system-priorities '(utf-8)
   mm-inline-large-images 'resize
   mm-inline-large-images-proportion 0.9
   mm-inline-text-html-with-images t
   sendmail-program "msmtpq-quiet"
   user-mail-address "tom.hinton@cse.org.uk"
   message-auto-save-directory nil))

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
  (defun shr-color-check-ignore-bg (shr-color-check fg bg)
    (funcall shr-color-check fg nil))
  (advice-add 'shr-color-check :around 'shr-color-check-ignore-bg))
