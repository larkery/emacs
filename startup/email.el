(use-package notmuch-agenda
  :defer t
  :commands notmuch-agenda-insert-part)

(use-package gnus-dired
  :commands turn-on-gnus-dired-mode
  :init
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  :custom
  (gnus-dired-mail-mode 'notmuch-user-agent)
  :config
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (bind-key "," 'gnus-dired-attach gnus-dired-mode-map))

(use-package org-add-font-lock-defaults
  :commands org-add-font-lock-defaults
  :init
  (add-hook 'notmuch-message-mode-hook 'org-add-font-lock-defaults))

(use-package org
  :commands orgtbl-mode
  :init
  (add-hook 'notmuch-message-mode-hook 'orgtbl-mode))

(use-package orgalist
  :ensure t
  :commands orgalist-mode
  :init
  (add-hook 'notmuch-message-mode-hook 'orgalist-mode))

(use-package org-mime
  :ensure t
  :commands org-mime-htmlize
  :bind
  (:map notmuch-message-mode-map
        ("C-c h" . org-mime-htmlize)
        ("C-c C-c" . maybe-htmlize-send-and-exit))
  :custom
  (org-mime-default-header
   "#+OPTIONS: latex:t toc:nil H:3 ^:{} broken-links:t
")
  (org-mime-beautify-quoted-mail nil)
  :config
  

  (defun narrow-to-message ()
    (interactive)
    (message-goto-body)
    (narrow-to-region
     (point)
     (save-excursion
       (point)
       (or (and (search-forward "<#part" nil t)
                (progn (beginning-of-line) t)
                (point))
           (point-max))))
    (goto-char (point-min)))

  (defun message-styled-p ()
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp
       (rx bol (| (: (* blank) (+ digit) "." blank)
                  (: (+ "*") blank)
                  (: (* blank) (any "-+") blank alphanumeric)
                  (: (group-n 1 (any "*/_~"))
                     (+ alphanumeric)
                     (backref 1))
                  (: (* blank) "|" (* nonl) "|" eol)
                  (: "[[/" (+ any) (| ".jpg" ".png" ".gif")"]]")
                  ))
       nil t)))

  (defun org-mime-maybe-htmlize ()
    (save-match-data
      (save-excursion
        (save-restriction
          (narrow-to-message)
          (mark-whole-buffer)
          (when (or (message-styled-p)
                    (y-or-n-p "Send as HTML?"))
            
            (message "Sending as HTML")
            (org-mime-htmlize))))))

  (defun org-mime--escaping-quote (args)
    (let ((text (car args))
          (rest (cdr args)))
      ;; transform the text
      (cons text rest)))

  (advice-add 'org-mime--export-string :filter-args #'org-mime--escaping-quote)

  (defun maybe-htmlize-send-and-exit ()
    (interactive)
    (org-mime-maybe-htmlize)
    (notmuch-mua-send-and-exit))

  (defun org-mime-pre-quotify (&rest _)
    (save-excursion
      (goto-char (point-min))
      (let ((qdepth 0)
            (qdepth* 0)
            (last-qlstart (make-marker)))
        (set-marker-insertion-type last-qlstart t)
        (while (not (eobp))
          (cond
           ((looking-at "^ *>+")
            (setq qdepth*
                  (progn (search-forward-regexp "^ *\\(>+\\)\\s-*" nil t)
                         (length (match-string 1))))
            
            (goto-char (match-beginning 0))
            (set-marker last-qlstart (point))
            (delete-forward-char (- (match-end 0) (point)))
            (when (looking-at "^\\*") (insert " ")))
           
           ((looking-at "^\\s-*$")
            (setq qdepth* qdepth))
           
           (t (setq qdepth* 0)))

          (cond
           ((> qdepth* qdepth)
            (dotimes (_ (- qdepth* qdepth))
              (insert "#+HTML: <blockquote>\n")))
           ((< qdepth* qdepth)
            (save-excursion
              (goto-char last-qlstart)
              (forward-line)
              (dotimes (_ (- qdepth qdepth*))
                (insert "#+HTML: </blockquote>\n")))))
          
          (setq qdepth qdepth*)
          (forward-line))
        (when (and (> qdepth 0)
                   (not (looking-at "^$")))
          (insert "\n"))
        (when (and last-qlstart (> qdepth 0))
          (save-excursion
            (goto-char last-qlstart)
            (forward-line)
            (dotimes (_ qdepth)
              (insert "#+END_QUOTE\n")))
          (set-marker last-qlstart nil))
        )))

  (defun org-mime-style-blockquote ()
    (org-mime-change-element-style
     "blockquote"
     "margin:0 0 0 .8ex;border-left:3px #ccc solid;padding-left:1ex"))

  (advice-add 'org-mime-htmlize :before 'org-mime-pre-quotify)
  (add-hook 'org-mime-html-hook 'org-mime-style-blockquote))

(use-package notmuch
  :commands notmuch
  :bind
  (("C-c m" . counsel-notmuch)
   :map notmuch-search-mode-map
   ("O" . notmuch-search-other-place)
   ("f" . notmuch-flag)
   ("d" . notmuch-delete)
   ("u" . notmuch-mark-read)
   
   ("g" . notmuch-refresh-this-buffer)
   ("@" . notmuch-search-person)
   ("z" . notmuch-tree-from-search-thread)
   ("RET" . notmuch-search-show-or-tree)
   :map notmuch-message-mode-map
   ("C-c f" . notmuch-switch-identity)
   :map notmuch-show-mode-map
   ("C-o" . open-url-at-point)
   ("d" . notmuch-delete)
   ("f" . notmuch-flag)
   ("u" . notmuch-mark-read)
   ("U" . notmuch-show-skip-to-unread)
   :map notmuch-tree-mode-map
   ("q" . notmuch-tree-quit-harder))
  :custom
  (notmuch-multipart/alternative-discouraged '("text/plain")) ;; prefer html?

  (notmuch-saved-searches
   '((:name "unread" :query "tag:unread" :key "u")
     (:name "inbox" :query "tag:inbox or tag:flagged" :key "i")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "NHM" :query "from:nhm.support@cse.org.uk" :key "N")
     ))

  (notmuch-tag-formats
   '(("unread" (propertize "•" 'face 'notmuch-tag-unread))
     ("flagged" (propertize tag 'face 'notmuch-tag-flagged)
      (notmuch-tag-format-image-data tag (notmuch-tag-star-icon)))
     ("low-importance" (propertize "_" 'face 'shadow))
     ("high-importance" (propertize "^" 'face 'error))
     ("normal-importance")
     ("medium-importance")

     ("home")
     ("work")
     ("sent")
     ("replied" "→")
     ("attachment" "@")
     ("inbox" "%")

     ))

  (notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ("deleted" . (:strike-through "red"))
     ("flagged" . notmuch-search-flagged-face)
     ))
  
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s ")
     ("authors" . "%-20s ")
     ("subject" . "%s ")
     ("tags" . "%s")))
  
  (notmuch-search-oldest-first nil)
  (notmuch-fcc-dirs
   '(("tom\\.hinton@cse\\.org\\.uk" . "\"cse/Sent Items\" +sent -inbox")
     ("larkery\\.com" . "\"fastmail/Sent Items\" +sent -inbox")))
  (notmuch-identities
   '("Tom Hinton <tom.hinton@cse.org.uk>" "Tom Hinton <t@larkery.com>"))
  (notmuch-draft-folders
   '(("tom\\.hinton@cse\\.org\\.uk" . "cse/Drafts")
     ("larkery\\.com" . "fastmail/Drafts")))

  (notmuch-address-selection-function
   (lambda
     (prompt collection initial-input)
     (completing-read prompt collection nil nil nil
                      (quote notmuch-address-history))))
  
  :config
  (require 'org-mime)
  (require 'notmuch-fancy-html)

  (defun notmuch-search-show-or-tree ()
    (interactive)
    (let* ((thread-id (notmuch-search-find-thread-id))
           (messages (notmuch-query-get-message-ids thread-id)))
      (if (> (length messages) 5)
          (notmuch-tree-from-search-thread)
        (notmuch-search-show-thread))))

  (defun notmuch-tree-quit-harder ()
    (interactive)
    (notmuch-tree-close-message-window)
    (kill-buffer (current-buffer)))
  
  (defun open-url-at-point (prefix)
    (interactive "P")
    (let* ((shr-url (get-text-property (point) 'shr-url))
           (the-url (or shr-url
                        (thing-at-point 'url)
                        (thing-at-point 'filename)
                        (browse-url-url-at-point)))
           
           (the-url (if prefix
                        (progn
                          (string-match "\\(.*\\)[\\/]" the-url)
                          (match-string 1 the-url))
                      the-url)))
      (browse-url the-url)))

  (defun notmuch-system-tag ()
    (if (member (system-name) '("limiting-factor"))
        "tag:work"
      "tag:home"))

  (defun notmuch-non-system-tag ()
    (if (member (system-name) '("limiting-factor"))
        "tag:home"
      "tag:work"))
  
  (defun notmuch-avoid-tag (query)
    (let ((req-tag (notmuch-system-tag)))
      (unless (and
               (not (eq this-command 'notmuch-search))
               query
               (string-match-p req-tag query))
        (concat req-tag " and "))))
  
  (defvar counsel-notmuch-history nil)

  (defun notmuch-search-person ()
    (interactive)
    (let* ((options (notmuch-address-options ""))
           (choice (ivy-completing-read
                    "Person: "
                    options
                    nil
                    nil
                    ;; (plist-get  :authors)
                    "" ;; TODO get author email addresses here? or stick them at the start?
                    )))
      (when choice
        (notmuch-search (format "from: %s or to:%s" choice choice)))))
  
  (defun counsel-notmuch ()
    (interactive)
    (let* ((search
            (ivy-read "Notmuch: "
                      (mapcar (lambda (x) (plist-get x :name)) notmuch-saved-searches)
                      :history 'counsel-notmuch-history
                      :caller 'counsel-notmuch-blah))
           (match (car (remove-if-not (lambda (x)
                                        (string= (plist-get x :name) search))
                                      notmuch-saved-searches)))
           (search (if match (plist-get match :query) search)))
      
      
      (notmuch-search (concat (notmuch-avoid-tag search) "(" search ")"))))
  
  (require 'notmuch-switch-identity)
  (fset 'notmuch-show-insert-part-text/calendar #'notmuch-agenda-insert-part)

  (defun in-home-directory (o &rest args)
    (let ((default-directory "~"))
      (apply o args)))

  (advice-add 'notmuch :around 'in-home-directory)

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


  (defun notmuch-toggle-tag (tags advance)
    (let* ((cur-tags
            (cl-case major-mode
              (notmuch-search-mode
               (notmuch-search-get-tags))

              (notmuch-show-mode
               (notmuch-show-get-tags))))
           (action (if (cl-intersection cur-tags tags :test 'string=) "-" "+"))
	   (arg (mapcar (lambda (x) (concat action x)) tags)))
      
      (cl-case major-mode
        (notmuch-search-mode
         (notmuch-search-tag arg)
         (when advance (notmuch-search-next-thread)))
        (notmuch-show-mode
         (notmuch-show-tag arg)
         (when advance (notmuch-show-next-matching-message))))))
  
  (defun notmuch-search-toggle-tag (&rest tags)
    (let* ((cur-tags (notmuch-search-get-tags))
	   (action (if (cl-intersection cur-tags tags :test 'string=) "-" "+"))
	   (arg (mapcar (lambda (x) (concat action x)) tags)))
      (notmuch-search-tag arg)))

  (defun notmuch-flag ()
    (interactive)
    (notmuch-toggle-tag '("flagged") t))
  
  (defun notmuch-delete ()
    (interactive)
    (notmuch-toggle-tag '("deleted") t))

  (defun notmuch-mark-read ()
    (interactive)
    (notmuch-toggle-tag '("unread") t))

  (defun notmuch-expand-calendar-parts (o msg part depth &optional hide)
    (funcall o
             msg part depth (and hide
                                 (not (string= (downcase (plist-get part :content-type))
                                               "text/calendar")))))
  
  (advice-add 'notmuch-show-insert-bodypart :around #'notmuch-expand-calendar-parts)


  (defun notmuch-show-skip-to-unread ()
    (interactive)
    (while (and (not (member "unread" (notmuch-show-get-tags)))
                (notmuch-show-goto-message-next)))
    (notmuch-show-message-visible (notmuch-show-get-message-properties) t)
    (recenter-top-bottom 0))

  (defvar notmuch-reply-sender-regexes
    (list (cons (regexp-quote "tom.hinton@cse.org.uk") "Tom Hinton")
          (cons (regexp-quote "larkery.com") "Tom Hinton")))
  
  (defun notmuch-mua-reply-guess-sender (o query-string &optional sender reply-all)
    (let ((sender (or sender
                      (let* ((to
                              (notmuch-query-map-threads
                               (lambda (m)
                                 (ietf-drums-parse-addresses
                                  (plist-get (plist-get m :headers) :To)))
                               (notmuch-call-notmuch-sexp
                                "show"
                                "--format=sexp"
                                "--format-version=4"
                                "--entire-thread=false"
                                "--body=false"
                                query-string)
                               )
                              )
                             (to (apply #'nconc to))
                             (case-fold-search t))
                        ;; TODO check notmuch filing rules as well?
                        (cl-loop for a in to
                                 thereis
                                 (cl-loop for i in notmuch-reply-sender-regexes
                                          when (string-match-p (car i) (car a))
                                          return (format "%s <%s>" (cdr i) (car a))))))))
      (funcall o query-string sender reply-all)))


  (advice-add 'notmuch-mua-reply :around 'notmuch-mua-reply-guess-sender)

  ;; don't colour in the whole line
  (defun notmuch-search-color-line-here ()
    (let ((face (plist-get (text-properties-at (point)) 'face)))
      (or (eq face 'notmuch-search-matching-authors)
          (eq face 'notmuch-search-subject)
          (eq face 'notmuch-search-date)
          (not face))))

  (defun notmuch-search-color-line-partially (o start end line-tag-list)
    (save-excursion
      (goto-char start)

      (let (npt (in (notmuch-search-color-line-here)) (last start))
        (while (and (setq npt (next-single-property-change (point) 'face nil end))
                    (< npt end))
          (goto-char npt)
          (let ((face (assoc 'face (text-properties-at (point)))))
            (if (notmuch-search-color-line-here)
                (unless in (setq in t last (point)))
              (when in
                (setq in nil)
                (funcall o last (point) line-tag-list)))))
        (when in
          (setq in nil)
          (funcall o last (point) line-tag-list)))))

  (advice-add 'notmuch-search-color-line :around #'notmuch-search-color-line-partially)

  (defun notmuch-read-query-suggest-tag (nrq prompt)
    (let* ((tag-to-hide
            (notmuch-avoid-tag (case major-mode
			         (notmuch-search-mode (notmuch-search-get-query))
			         (notmuch-show-mode (notmuch-show-get-query))
			         (notmuch-tree-mode (notmuch-tree-get-query))))))
      
      (if tag-to-hide
          (let ((do-read-from-minibuffer (symbol-function 'read-from-minibuffer)))
            (cl-letf (((symbol-function 'read-from-minibuffer)
                       (lambda (prompt &optional initial-contents keymap read
                                       hist default-value inherit-input-method)
                         (funcall do-read-from-minibuffer
                                  prompt
                                  (concat tag-to-hide initial-contents)
                                  keymap read hist default-value inherit-input-method))))
              (funcall nrq prompt)))
        (funcall nrq prompt))))

  (advice-add 'notmuch-read-query :around #'notmuch-read-query-suggest-tag)

  (defun notmuch-search-other-place ()
    (interactive)
    (let ((search (notmuch-search-get-query))
          (here (notmuch-system-tag))
          (there (notmuch-non-system-tag)))
      
      (notmuch-search
       (cond
        ((string-match-p (regexp-quote here) search)
         (replace-regexp-in-string (regexp-quote here) there search))
        ((string-match-p (regexp-quote there) search)
         (replace-regexp-in-string (regexp-quote there) here search))
        (t
         (concat here " AND (" search ")")))))))

(use-package notmuch-attachment-list
  :after notmuch
  :bind (:map notmuch-search-mode-map
         ("A" . notmuch-list-attachments)
         :map notmuch-show-mode-map
         ("A" . notmuch-list-attachments)
         :map notmuch-tree-mode-map
         ("A" . notmuch-list-attachments)))

(use-package message
  :defer t
  :custom
  (mml-enable-flowed nil)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-sendmail-envelope-from 'header)
  (message-fill-column nil)
  (mm-coding-system-priorities '(utf-8))
  (mm-inline-override-types '("image/tiff"))
  (mm-inline-large-images 'resize)
  (mm-inline-large-images-proportion 0.9)
  (mm-inline-text-html-with-images t)
  (sendmail-program "msmtpq-quiet")
  (user-mail-address "tom.hinton@cse.org.uk")
  (message-auto-save-directory nil)
  (message-interactive nil)
  (message-kill-buffer-on-exit t)
  (message-default-charset 'utf-8)
  (user-full-name "Tom Hinton")
  (message-signature 'message-signature-select-by-from)
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-yank-empty-prefix "")
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
        (when (string-match-p (caar choices) from)
          (setq result (cdar choices)))
        (setq choices (cdr choices)))
      result))

  (defun message-attach-at-end (o file &rest args)
    (save-excursion
      (goto-char (point-max))
      (let ((dir (file-name-directory file)))
        (when dir (setq default-directory dir)))
      (apply o file args)))

  (advice-add 'mml-attach-file :around 'message-attach-at-end)
  
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
  :custom
  (shr-color-visible-luminance-min 75)
  (shr-use-fonts nil)
  :config

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

(use-package bbdb
  :defer t
  :ensure t
  :commands bbdb
  :custom
  (bbdb-file "~/notes/bbdb")
  (bbdb-default-country nil)
  :config
  
  (defun bbdb-use-completing-read-default ()
    (setq-local completing-read-function 'completing-read-default))
  
  (add-hook 'bbdb-mode-hook 'bbdb-use-completing-read-default))
