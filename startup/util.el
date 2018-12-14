(use-package magit
  :bind ("C-c g" . magit-status)
  :ensure t
  :commands magit-status)

(use-package vc
  :defer t
  :config
  (add-hook 'vc-annotate-mode-hook 'vc-annotate-toggle-annotation-visibility))

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :diminish smartparens-mode
  :ensure t
  :commands smartparens-mode
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp) ;; navigation
              ("C-M-b" . sp-backward-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-p" . sp-backward-down-sexp)
              ("C-M-n" . sp-up-sexp)
              ("C-%" . sp-splice-sexp) ;; depth-changing commands
              ("C-^" . sp-splice-sexp-killing-around)
              ("C-(" . sp-wrap-with-paren) ;; barf/slurp
              ("C-\"" . sp-forward-slurp-sexp)
              ("C-)" . sp-forward-barf-sexp)
              ("C-~" . sp-convolute-sexp)
              ("C-&" . sp-rewrap-cycle)
              ("C-|" . sp-split-sexp) ;; misc
              ("C-M-;" . sp-comment-or-uncomment-sexp)
              ("C-#" . sp-reindent-toplevel)
              ("C-;" . move-past-close-reindent-and-flash))
  :init
  
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'show-smartparens-mode)

  :config

  (defun sp-wrap-with-paren ()
    (interactive)
    (sp-wrap-with-pair "("))

  (defun sp-rewrap-cycle ()
    (interactive)
    (let ((delim (plist-get (sp-get-enclosing-sexp) :op)))
      (pcase delim
        ("(" (sp-rewrap-sexp '("[" . "]")))
        ("[" (sp-rewrap-sexp '("{" . "}")))
        ("{" (sp-rewrap-sexp '("(" . ")")))
        ("_" (sp-wrap-with-pair "(")))))
  
  (defun sp-reindent-toplevel ()
    (interactive)
    (save-excursion
      (mark-defun)
      (indent-region (point) (mark))))

  (defun move-past-close-reindent-and-flash ()
    (interactive)
    
    (move-past-close-and-reindent)
    (pulse-momentary-highlight-region
     (save-excursion
       (sp-backward-sexp)
       (point))
     (point)))
  
  
  (defun sp-comment-or-uncomment-sexp ()
    (interactive)
    (comment-or-uncomment-region
     (point)
     (save-mark-and-excursion
      (sp-forward-sexp)
      (unless (looking-at ".") (insert "\n"))
      (point))))
  
  (require 'smartparens-config)
  (setq-default sp-autoskip-closing-pair 'always-end))

(use-package paredit
  :defer t
  :config
  (disable-paredit-mode))

(use-package electric
  :defer t
  :init
  (add-hook 'prog-mode-hook 'electric-indent-mode))

(use-package winner
  :defer nil
  :bind (("C-<" . winner-undo)
	 ("C->" . winner-redo))
  :config
  (winner-mode 1))

(use-package projectile
  :diminish
  :ensure t
  :demand t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-global-mode 1)

  (bind-keys
   :map projectile-mode-map
   ("C-c p s s" . counsel-ag)
   ("C-c p s a" . projectile-ag)
   ("C-x C-b" . projectile-or-global-ibuffer)
   ("M-s p" . projectile-ag))
  
  ;; (define-key projectile-mode-map (kbd projectile-keymap-prefix) 'projectile-command-map)

  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired)
  (defun projectile-or-global-ibuffer (prefix)
    (interactive "P")
    (if (projectile-project-p)
        (projectile-ibuffer prefix)
      (ibuffer))))

(use-package pcre2el
  :diminish pcre-mode
  :ensure t
  :config
  (pcre-mode t))

(use-package replace
  :bind ("M-s o" . occur)
  :defer t
  :commands occur)

(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . 'anzu-query-replace)
   ([remap query-replace-regexp] . 'anzu-query-replace-regexp)
   ("<f5>" . anzu-query-replace-regexp)
   ("M-s N" . anzu-replace-at-cursor-thing)
   ("M-s n" . anzu-query-replace-at-cursor)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  
  :defer t
  :commands (anzu-query-replace
             anzu-query-replace-at-cursor
             anzu-isearch-query-replace)
  :config
  (defun my-anzu-wangle-minibuffer-input (f buf beg end use-re overlay-limit)
    (if (and use-re pcre-mode)
        (let ((-minibuffer-contents (symbol-function 'minibuffer-contents)))
          (cl-letf (((symbol-function 'minibuffer-contents)
                     (lambda ()
                       (let ((mc (funcall -minibuffer-contents)))
                         (condition-case nil
                             (rxt-pcre-to-elisp mc)
                           (error "";; mc
                                  ))))))
            (funcall f buf beg end use-re overlay-limit)))

      (funcall f buf beg end use-re overlay-limit)))

  (defun my-anzu-pcre-mode (f prompt beg end use-re overlay-limit)
    (if (and use-re pcre-mode)
        (let ((res (funcall f (concat prompt " (PCRE)") beg end use-re overlay-limit)))
          (condition-case nil
              (rxt-pcre-to-elisp res)
            (error (error "'%s' is invalid regexp." res))))
      (funcall f prompt beg end use-re overlay-limit)))

  (advice-add 'anzu--check-minibuffer-input :around #'my-anzu-wangle-minibuffer-input)
  (advice-add 'anzu--query-from-string :around #'my-anzu-pcre-mode)

  ;;(advice-remove 'anzu--check-minibuffer-input #'my-anzu-wangle-minibuffer-input)
  ;;(advice-remove 'anzu--query-from-string #'my-anzu-pcre-mode)
  
  (setq anzu-mode-lighter ""))

(use-package dumb-jump
  :ensure t
  :defer t
  :bind (("C-." . dumb-jump-go) ;; Go to Symbol, ish
         ("C-," . dumb-jump-back)))

(use-package which-key
  :diminish ""
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.75))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name
          try-complete-file-name-partially
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))


(use-package ibuffer
  :bind (:map ibuffer-mode-map
              ("/" . hydra-ibuffer-filter/body))
  :config
  (setq ibuffer-formats
        '((mark modified read-only
                " "
                (name 32 32 :left :elide)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  (define-ibuffer-filter project
      "Toggle current view to buffers with projectile root dir QUALIFIER."
    (:description "project root dir"
                  :reader (read-regexp "Filter to project (regexp): "))
    (ibuffer-awhen (with-current-buffer buf
                     (let ((projectile-require-project-root nil))
                       (projectile-project-root)))
      (string-match-p qualifier it)))

  (define-ibuffer-filter host
      "Toggle current view to buffers with tramp host QUALIFIER."
    (:description "remote host"
                  :reader (read-regexp "Filter to host (regexp): "))
    (ibuffer-awhen (with-current-buffer buf
                     (let ((f (or (buffer-file-name)
                                  default-directory)))
                       (if (file-remote-p f)
                           (tramp-file-name-host (tramp-dissect-file-name f))
                         "local")))
      (string-match-p qualifier it)))

  (defun ibuffer-group-by-project ()
    (interactive)
    (setq ibuffer-filter-groups
          (let ((projects
                 (ibuffer-remove-duplicates
                  (cl-loop for b being the buffers
                           collect (with-current-buffer b
                                     (let ((projectile-require-project-root nil))
                                       (projectile-project-root)))))))
            (cl-loop for p in projects
                     when p
                     collect
                     (cons p
                           `((project . ,(regexp-quote p)))))))
    (ibuffer-update nil))

  (defun ibuffer-group-by-host ()
    (interactive)
    (setq ibuffer-filter-groups
          (let ((projects
                 (ibuffer-remove-duplicates
                  (cl-loop for b being the buffers
                           collect (with-current-buffer b
                                     (let ((f (or (buffer-file-name)
                                                  default-directory)))
                                       (if (file-remote-p f)
                                           (tramp-file-name-host (tramp-dissect-file-name f))
                                         "local")))))))
            (cl-loop for p in projects
                     when p
                     collect
                     (cons p
                           `((project . ,(regexp-quote p)))))))
    (ibuffer-update nil)))

(use-package hydra
  :commands hydra-ibuffer-filter/body
  :config

  (defhydra hydra-ibuffer-group (:color amaranth :columns 4 :exit t)
    "Group by"
    ("p" ibuffer-group-by-project "project")
    ("h" ibuffer-group-by-host "host")
    ("/" ibuffer-clear-filter-groups "clear"))
 
    
  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4 :exit t)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("p" ibuffer-filter-by-project "project")
    ("h" ibuffer-filter-by-host "host")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("g" hydra-ibuffer-group/body "group")))

(use-package rcirc-notify
  :defer t
  :ensure t
  :config
  (rcirc-notify-add-hooks)
  (setq rcirc-notify-keywords '("tom" "tomhinton" "hinton")))

(use-package rcirc
  :defer t
  :config
  (require 'rcirc-notify)
  (setq rcirc-server-alist
        `(("larkery.com"
           :port 7778
           :user-name "hinton"
           :nick "TomHinton"
           :encryption tls
           :password ,(funcall (plist-get (car
                                           (auth-source-search :host "larkery.com"  :port "7778")) :secret))
           ))

        rcirc-fill-flag nil)

  (add-hook 'rcirc-mode-hook 'visual-line-mode)
  (add-hook 'rcirc-mode-hook (lambda () (setq wrap-prefix "      "))))

(use-package auth-source-pass
  :ensure t
  :after auth-source
  :config
  (auth-source-pass-enable)
  )

(use-package god-mode
  :ensure t
  :bind ("<escape>" . god-local-mode)
  :diminish god-local-mode
  :config

  (define-key god-local-mode-map (kbd ".") 'repeat)
  (add-to-list 'god-exempt-major-modes 'notmuch-search-mode)
  (add-to-list 'god-exempt-major-modes 'notmuch-show-mode)

  (defvar god-mode-modeline-text nil)
  (make-variable-buffer-local 'god-mode-modeline-text)

  (setq god-mode-modeline-text nil)
  
  (push '(:propertize god-mode-modeline-text
                     face (:foreground "white" :background "red")
                     )
        mode-line-misc-info)

  (defun god-local-mode-lighter ()
    (setq god-mode-modeline-text
          (and god-local-mode " G ")))
  
  (add-hook 'god-local-mode-hook
            'god-local-mode-lighter)
  
  )

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(defun mark-symbol-or-minibuffer-it ()
  (interactive)
  (if (minibufferp)
      (insert
       (with-current-buffer
           (window-buffer (minibuffer-selected-window))
         (when-let ((b (bounds-of-thing-at-point 'symbol)))
           (buffer-substring-no-properties
            (car b) (cdr b))
           )
         ))
    (when-let ((b (bounds-of-thing-at-point 'symbol)))
      (set-mark (car b))
      (goto-char (cdr b)))
    )
  )

(bind-key "C-'" 'mark-symbol-or-minibuffer-it)

(use-package multiple-cursors
  :defer t
  :bind (("C-/" . mc/mark-more-like-this-extended)
         ("C-M-/" . mc/mark-all-dwim))
  :config
  ;; (defvar was-in-composable-mode nil)

  ;; (make-variable-buffer-local 'was-in-composable-mode)

  ;; (defun toggle-composable-mode ()
  ;;   (if multiple-cursors-mode
  ;;       (progn
  ;;         (setq-local was-in-composable-mode composable-mode)
  ;;         (composable-mode 0))
  ;;     (when was-in-composable-mode
  ;;       (composable-mode 1))))

  
  ;; (add-hook 'multiple-cursors-mode-hook
  ;;           #'toggle-composable-mode)
  )

(use-package edit-as-root
  :bind ("C-x C-a" . edit-as-root))

(use-package transpose-frame
  :ensure t
  :bind ("C-M-o" . transpose-frame))

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :ensure t
  :bind (:map symbol-overlay-mode-map
              ("M-p" . symbol-overlay-jump-prev)
              ("M-n" . symbol-overlay-jump-next))
  :commands symbol-overlay-mode
  :config
  (setq symbol-overlay-idle-time 0.75)
  :init
  (add-hook 'prog-mode-hook 'symbol-overlay-mode))


(use-package browse-url
  :config
  (setq browse-url-generic-program "xdg-open"
        browse-url-browser-function 'browse-url-generic))

;; (use-package composable
;;   :diminish
;;   :config
;;   (composable-mode 1)
;;   (composable-mark-mode 1)
;;   (bind-key "M-;" nil composable-mode-map))


(defun insert-file-path ()
  (interactive)
  (push-mark)
  (let ((path (read-file-name "Insert path: ")))
    (when path
      (insert "\"")
      (insert path)
      (insert "\""))))

(bind-key "C-c f" #'insert-file-path)

(use-package yasnippet
  :ensure t
  :defer t
  :commands yas/minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas/minor-mode))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package goto-chg
  :ensure t
  :bind ("C-=" . goto-last-change))

(use-package ag
  :ensure t
  :config
  (setq ag-arguments '("--smart-case")
        ag-highlight-search t))

