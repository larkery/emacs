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

  (bind-key "," 'gnus-dired-attach gnus-dired-mode-map)
  (bind-key "E" 'gnus-dired-attach gnus-dired-mode-map))

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
  :bind
  (:map orgalist-mode-map
        ("C-c l" . org-insert-link))
  
  :init
  (add-hook 'notmuch-message-mode-hook 'orgalist-mode))


(use-package org-mime
  :ensure t
  :commands org-mime-htmlize
  :custom
  (org-mime-export-ascii 'utf-8)
  (org-mime-beautify-quoted-mail-p t)
  )

;; (use-package org-mime
;;   :ensure t
;;   :commands org-mime-htmlize
;;   :bind
;;   (:map notmuch-message-mode-map
;;         ("C-c h" . org-mime-htmlize)
;;         ("C-c C-c" . maybe-htmlize-send-and-exit)
;;         ("C-c C-s" . notmuch-mua-send))
  
;;   :custom
;;   (org-mime-beautify-quoted-mail-p nil)
  
;;   :config

;;   (setq org-mime-export-options
;;         '(:with-author
;;           nil
;;           :with-toc nil
;;           :with-sub-superscript nil
;;           :with-latex dvipng
;;           ))
  
;;   (defun narrow-to-message ()
;;     (interactive)
;;     (message-goto-body)
;;     (narrow-to-region
;;      (point)
;;      (save-excursion
;;        (point)
;;        (or (and (search-forward-regexp
;;                  (rx "<#" (| "multipart" "part" "external" "mml"))
;;                  nil t)
;;                 (progn (beginning-of-line) t)
;;                 (point))
;;            (point-max))))
;;     (goto-char (point-min)))

;;   (defun org-mime-maybe-htmlize ()
;;     (save-match-data
;;       (save-excursion
;;         (save-restriction
;;           (narrow-to-message)
;;           (mark-whole-buffer)
;;           (org-mime-htmlize)))))

;;   ;; just do html part.
;;   (defun org-mime-multipart (plain html &optional images)
;;     "Markup a multipart/alternative with HTML alternatives.
;; If html portion of message includes IMAGES they are wrapped in multipart/related part."
;;     (concat (when images "<#multipart type=related>")
;;             "<#part type=text/html>\n"
;;             (if org-mime-beautify-quoted-mail-p
;;                 (org-mime-beautify-quoted html)
;;               html)
;;             images
;;             (when images "<#/multipart>\n")))

;;   (defun maybe-htmlize-send-and-exit (prefix)
;;     (interactive "P")
;;     (unless prefix (org-mime-maybe-htmlize))
;;     (notmuch-mua-send-and-exit))

;;   (defun org-mime-pre-quotify (&rest _)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (let ((qdepth 0)
;;             (qdepth* 0)
;;             (last-qlstart (make-marker)))
;;         (set-marker-insertion-type last-qlstart t)
;;         (while (not (eobp))
;;           (cond
;;            ((looking-at "^ *>+")
;;             (setq qdepth*
;;                   (progn (search-forward-regexp "^ *\\(>+\\)\\s-*" nil t)
;;                          (length (match-string 1))))
            
;;             (goto-char (match-beginning 0))
;;             (set-marker last-qlstart (point))
;;             (delete-forward-char (- (match-end 0) (point)))
;;             (when (looking-at "^\\*") (insert " ")))
           
;;            ((looking-at "^\\s-*$")
;;             (setq qdepth* qdepth))
           
;;            (t (setq qdepth* 0)))

;;           (cond
;;            ((> qdepth* qdepth)
;;             (dotimes (_ (- qdepth* qdepth))
;;               (insert "#+HTML: <blockquote>\n")))
;;            ((< qdepth* qdepth)
;;             (save-excursion
;;               (goto-char last-qlstart)
;;               (forward-line)
;;               (dotimes (_ (- qdepth qdepth*))
;;                 (insert "#+HTML: </blockquote>\n")))))
          
;;           (setq qdepth qdepth*)
;;           (forward-line))
;;         (when (and (> qdepth 0)
;;                    (not (looking-at "^$")))
;;           (insert "\n"))
;;         (when (and last-qlstart (> qdepth 0))
;;           (save-excursion
;;             (goto-char last-qlstart)
;;             (forward-line)
;;             (dotimes (_ qdepth)
;;               (insert "#+HTML: </blockquote>\n")))
;;           (set-marker last-qlstart nil))
;;         )))

;;   (defun org-mime-style-blockquote ()
;;     (org-mime-change-element-style
;;      "blockquote"
;;      "margin:0 0 0 .8ex;border-left:3px #ccc solid;padding-left:1ex"))

;;   (advice-add 'org-mime-htmlize :before 'org-mime-pre-quotify)
;;   (add-hook 'org-mime-html-hook 'org-mime-style-blockquote))


(use-package notmuch
  :commands notmuch
  :bind
  (("C-c m" . counsel-notmuch)
   ("C-x m" . notmuch-mua-new-mail)
   :map notmuch-search-mode-map
   ("O" . notmuch-search-other-place)
   ("f" . notmuch-flag)
   ("d" . notmuch-delete)
   ("u" . notmuch-mark-read)
   ("i" . notmuch-mark-inbox)
   ("," . notmuch-mark-for-operation)
   ("." . notmuch-do-operation)
   ("g" . notmuch-refresh-this-buffer)
   ("@" . notmuch-search-person)
   ("z" . notmuch-tree-from-search-thread)
   ("RET" . notmuch-search-show-or-tree)
   ;; ("G" . notmuch-start-idle)
   :map notmuch-message-mode-map
   ("C-c f" . notmuch-switch-identity)
   ("C-c s" . message-remove-or-update-signature)
   ;; ("C-c z" . message-kill-remaining-quote)
   ;; ("C-c i" . message-insert-or-toggle-importance)
   ;; ("C-c q" . message-quotify-region)
   ("C-c C-s" . nil)
   ("M-n" . message-next-thing)
   :map notmuch-show-mode-map
   ("C-o" . open-url-at-point)
   ("d" . notmuch-delete)
   ("f" . notmuch-flag)
   ("U" . notmuch-mark-read)
   ("u" . notmuch-skip-to-unread)
   ("v" . notmuch-show-view-part)
   ("S" . notmuch-show-save-part)
   :map notmuch-tree-mode-map
   ("q" . notmuch-tree-quit-harder)
   ("u" . notmuch-skip-to-unread))
    
  :custom
  (notmuch-multipart/alternative-discouraged '("text/plain")) ;; prefer html?

  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox or tag:flagged" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "recent"
            :query "date:\"this week\""
            :key "r"
            )
     ))

  (notmuch-tag-formats
   '(("unread"  (propertize "u" 'face 'highlight))
     ("flagged" (propertize "f" 'face 'highlight))     
     ("low-importance" (propertize "_" 'face 'shadow))
     ("high-importance" (propertize "^" 'face 'error))
     ("normal-importance")
     ("medium-importance")
     ("meeting" (propertize "m" 'face 'highlight))

     ("home")
     ("work")
     ("sent")
     ("replied" (propertize "r" 'face 'highlight))
     ("attachment" (propertize "a" 'face 'highlight))
     ("inbox" (propertize "i" 'face 'highlight))
     ("deleted" (propertize "d"  'face '(error (:inverse-video t))))
     ))

  (notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ("flagged" . notmuch-search-flagged-face)))
  
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s ")
     ("authors" . "%-20s ")
     ("subject" . "%s ")
     ("tags" . "(%s)")))
  
  (notmuch-search-oldest-first nil)
  (notmuch-fcc-dirs
   '(("tom\\.hinton@cse\\.org\\.uk" . "\"cse/Sent Items\" +sent -inbox")
     ("larkery\\.com" . "\"fm/Sent Items\" +sent -inbox")))
  (notmuch-identities
   '("Tom Hinton <tom.hinton@cse.org.uk>" "Tom Hinton <t@larkery.com>"))
  (notmuch-draft-folders
   '(("tom\\.hinton@cse\\.org\\.uk" . "cse/Drafts")
     ("larkery\\.com" . "fm/Drafts")))

  (notmuch-address-selection-function
   (lambda
     (prompt collection initial-input)
     (completing-read prompt collection nil nil nil
                      (quote notmuch-address-history))))
  
  :config
  (require 'org-mime)
  ;; (require 'notmuch-fancy-html)

  ;; (defun message-quotify-region ()
  ;;   (interactive)
  ;;   (if (region-active-p)
  ;;       (message-quote-here)
  ;;     (message-split-quote-here)))

  
  (defun message-next-thing ()
    (interactive)

    (cond
     ((message-in-body-p)
      (let ((here (point)))
        (message-goto-signature)
        (when (= here (point))
          (goto-char (point-min))
          (search-forward-regexp (rx bol (* alphanumeric) ": ") nil t))))
     
     ((not (search-forward-regexp (rx bol (* alphanumeric) ": ") nil t))
      (message-goto-body))))
  
  ;; (defun message-mark-end-quote ()
  ;;   (interactive)
  ;;   (unless (region-active-p)
  ;;     (set-mark (point))
  ;;     (activate-mark))
  ;;   (search-forward-regexp
  ;;    (rx bol "#+HTML: </blockquote>") nil t))


  ;; (defun message-kill-remaining-quote ()
  ;;   (interactive)
  ;;   (when (looking-at (rx bol "#+HTML: </blockquote>"))
  ;;     (kill-line))
  ;;   (message-mark-end-quote)
  ;;   (beginning-of-line)
  ;;   (kill-region (point) (mark)))

  
  ;; (defun message-split-quote-here ()
  ;;   (interactive)
  ;;   (insert "#+HTML: </blockquote>\n")
  ;;   (save-excursion
  ;;     (insert "\n#+HTML: <blockquote>\n")))


  ;; (defun message-quote-here ()
  ;;   (interactive)
  ;;   (if (region-active-p)
  ;;       (progn
  ;;         (save-excursion
  ;;           (goto-char (region-beginning))
  ;;           (beginning-of-line)
  ;;           (insert "#+HTML: <blockquote>\n"))
  ;;         (save-excursion
  ;;           (goto-char (region-end))
  ;;           (end-of-line)
  ;;           (insert "\n#+HTML: </blockquote>\n")))))

    
  (defun notmuch-insert-image-link ()
    (interactive)
    (let ((file (read-file-name "Image: ")))
      (when file
        (insert "[[" file "]]\n"))))

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
    (require 'cl-lib)
    (let* ((search
            (ivy-read "Notmuch: "
                      (mapcar (lambda (x) (plist-get x :name)) notmuch-saved-searches)
                      :history 'counsel-notmuch-history
                      :caller 'counsel-notmuch-blah
                      :keymap (let ((km (make-sparse-keymap)))
                                (bind-key "TAB" #'notmuch-read-query-hydra/body km)
                                km)))
           (match (car (cl-remove-if-not (lambda (x)
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

  (defun notmuch-mark-for-operation ()
    (interactive)
    (notmuch-toggle-tag '("✦") t))

  (defun notmuch-do-operation ()
    (interactive)
    (notmuch-search "tag:✦")
    (mark-whole-buffer))
    
  (defun notmuch-flag ()
    (interactive)
    (notmuch-toggle-tag '("flagged") t))
  
  (defun notmuch-delete ()
    (interactive)
    (notmuch-toggle-tag '("deleted") nil))

  (defun notmuch-mark-inbox ()
    (interactive)
    (notmuch-toggle-tag '("inbox") t))

  (defun notmuch-mark-read ()
    (interactive)
    (notmuch-toggle-tag '("unread") t))

  (defun notmuch-expand-calendar-parts (o msg part depth &optional hide)
    (funcall o
             msg part depth (and hide
                                 (not (string= (downcase (plist-get part :content-type))
                                               "text/calendar")))))

  (advice-add 'notmuch-show-insert-bodypart :around #'notmuch-expand-calendar-parts)

  (defun notmuch-skip-to-unread ()
    (interactive)
    (cl-case major-mode
      ;; todo find out if we are in a tree but in the show window
      (notmuch-show-mode
       (while (and (not (member "unread" (notmuch-show-get-tags)))
                   (notmuch-show-goto-message-next)))
       (notmuch-show-message-visible (notmuch-show-get-message-properties) t)
       (recenter-top-bottom 0))
      (notmuch-tree-mode
       (while (and (not (member "unread" (notmuch-tree-get-tags)))
                   (not (eobp)))
         (forward-line))
       (notmuch-tree-show-message nil))))

  (defvar mail-address-account-regexps
    `((,(regexp-quote "tom.hinton@cse.org.uk") "Tom Hinton" "cse")
      (,(regexp-quote "@larkery.com") "Tom Hinton" "fm")))

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
                                query-string)))
                             (to (apply #'nconc to))
                             (case-fold-search t))
                        ;; TODO check notmuch filing rules as well?
                        (cl-loop for a in to
                                 thereis
                                 (cl-loop for addr in mail-address-account-regexps
                                          when (string-match-p (car addr) (car a))
                                          return (format
                                                  "%s <%s>"
                                                  (nth 1 addr)
                                                  (car a))))))))
      (funcall o query-string sender reply-all)))


  (advice-add 'notmuch-mua-reply :around 'notmuch-mua-reply-guess-sender)

  (defun message-sendmail-add-account (o &rest args)
    (let* ((from (message-sendmail-envelope-from))

           (account (cl-loop for addr in mail-address-account-regexps
                             when (string-match-p (car addr) from)
                             return (nth 2 addr)))
           
           (message-sendmail-extra-arguments
            (if account
                (cons
                 "-a"
                 (cons
                  account 
                  message-sendmail-extra-arguments))
              message-sendmail-extra-arguments)))
      
      (apply o args)))

  (advice-add 'message-send-mail-with-sendmail :around 'message-sendmail-add-account)
    
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
         (concat here " AND (" search ")"))))))


  ;; (defun notmuch-start-idle ()
  ;;   (interactive)
  ;;   (if-let ((existing-buffer (get-buffer "*idle*")))
  ;;       (display-buffer-at-bottom existing-buffer nil)
  ;;     (async-shell-command "idle" "*idle*"))
    
  ;;   )

  )


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

  (message-send-mail-function 'message-send-mail-with-sendmail)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header)
  (message-sendmail-f-is-evil nil)
  (mail-specify-envelope-from t)
  (message-fill-column nil)
  (mm-coding-system-priorities '(utf-8))
  (mm-inline-override-types '("image/tiff" "application/zip"))
  (mm-inline-large-images 'resize)
  (mm-inline-large-images-proportion 0.9)
  (mm-inline-text-html-with-images t)
  (sendmail-program "msmtpq")
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
        "Dr. Tom Hinton (📞 0117 934 1455)")))
  
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

  (advice-add 'mml-attach-file :around 'message-attach-at-end))

(use-package mailcap
  :defer t
  :config
  (setq mailcap-user-mime-data
        '(((viewer . "xdg-open %s")
           (type . ".+")))))

(use-package dnd
  :config
  (setq dnd-protocol-alist nil)
  (setq-default dnd-protocol-alist nil))

(use-package shr
  :defer t
  :custom
  (shr-color-visible-luminance-min 75)
  (shr-use-fonts nil)
  (shr-bullet "- ")
  (shr-discard-aria-hidden t)
  (shr-hr-line 45)
  :config

  (defun shr-ensure-paragraph ()
    (unless (bobp)
      (let ((prefix (get-text-property (line-beginning-position)
                                       'shr-prefix-length)))
        (cond
         ((and (bolp)
               (save-excursion
                 (forward-line -1)
                 (looking-at " *$")))
          ;; We're already at a new paragraph; do nothing.
          )
         ((and prefix
               (= prefix (- (point) (line-beginning-position))))
          ;; Do nothing; we're at the start of a <li>.
          )
         ((save-excursion
            (beginning-of-line)
            ;; If the current line is totally blank, and doesn't even
            ;; have any face properties set, then delete the blank
            ;; space.
            (and (looking-at " *$")
                 (not (get-text-property (point) 'face))
                 (not (= (next-single-property-change (point) 'face nil
                                                      (line-end-position))
                         (line-end-position)))))
          (delete-region (match-beginning 0) (match-end 0)))
         ;; Insert new paragraph.
         (t
          (insert "\n"))))))
  
  (defun shr-colorise-region-ignore-bg
      (shr-colorise-region start end fg &optional bg)
    (funcall shr-colorise-region start end fg nil))
  
  (advice-add 'shr-colorize-region :around 'shr-colorise-region-ignore-bg)
;  (advice-remove 'shr-colorize-region 'shr-colorise-region-ignore-bg)

  ;; (defun disabling-gc (o &rest args)
  ;;   (let* (;; (start (current-time))

  ;;          (gc-cons-threshold (* 100 gc-cons-threshold))
  ;;          (result (apply o args))
  ;;          ;; (end (current-time))

  ;;          )
  ;;     ;; (message "%.2f" (float-time (subtract-time end start)))

  ;;     result))


  ;; (advice-add 'shr-insert-document :around 'disabling-gc)
  )
