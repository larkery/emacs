(use-package magit
  :bind ("C-c g" . magit-status)
  :ensure t
  :commands magit-status)

(use-package smartparens
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
              ("C-(" . sp-wrap-or-cycle) ;; barf/slurp
              ("C-\"" . sp-forward-slurp-sexp)
              ("C-)" . sp-forward-barf-sexp)
              ("C-~" . sp-convolute-sexp)
              ("C-|" . sp-split-sexp) ;; misc
              ("C-M-;" . sp-comment-or-uncomment-sexp)
              ("C-<tab>" . sp-reindent-toplevel)
              ("C-;" . move-past-close-and-reindent))
  :init
  
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'show-smartparens-mode)

  :config

  (defun sp-wrap-or-cycle ()
    (interactive)
    (if (eq last-command 'sp-wrap-or-cycle)
        (let ((delim (plist-get (sp-get-enclosing-sexp)
                                :op
                                )))
          (pcase delim
            ("(" (sp-rewrap-sexp '("[" . "]")))
            ("[" (sp-rewrap-sexp '("{" . "}")))
            ("{" (sp-rewrap-sexp '("(" . ")")))
            ("_" (sp-wrap-with-pair "("))))
      
      (sp-wrap-with-pair "("))
    
    )
  
  (defun sp-reindent-toplevel ()
    (interactive)
    (save-excursion
      (mark-defun)
      (indent-region (point) (mark))))
  
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
  :bind (:map projectile-mode-map
              ("C-c p s s" . counsel-ag)
              ("C-c p s a" . projectile-ag))
  :ensure t
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired))

(use-package pcre2el
  :ensure t
  :config
  (pcre-mode t))

(use-package replace
  :bind ("M-s o" . occur-at-point)
  :defer t
  :commands (occur)
  :init
  (defun occur-at-point (regexp)
    (interactive (list (read-regexp
                        "List lines matching regexp"
                        (cons
                         (thing-at-point 'symbol)
                         regexp-history))))
    (occur regexp current-prefix-arg)))

(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . 'anzu-query-replace)
   ([remap query-replace-regexp] . 'anzu-query-replace-regexp)
   ("M-s r" . anzu-query-replace-regexp)
   ("M-s N" . anzu-replace-at-cursor-thing)
   ("M-s n" . anzu-query-replace-at-cursor)
   :map isearch-mode-map
        ([remap isearch-query-replace] . anzu-isearch-query-replace)
        ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
        )
  
  :defer t
  :commands (anzu-query-replace
             anzu-query-replace-regexp
             anzu-query-replace-at-cursor
             anzu-isearch-query-replace
             anzu-isearch-query-replace-regexp
             )
  :config
  (setq anzu-mode-lighter "")

  (defun my-anzu-wangle-minibuffer-input (f buf beg end use-re overlay-limit)
    (if (and use-re pcre-mode)
        (let ((-minibuffer-contents (symbol-function 'minibuffer-contents)))
          (flet ((minibuffer-contents
                  ()
                  (let ((mc (funcall -minibuffer-contents)))
                    (condition-case nil
                        (rxt-pcre-to-elisp mc)
                      (error mc)))
                  ))
            (funcall f buf beg end use-re overlay-limit)))

      (funcall f buf beg end use-re overlay-limit)))

  (defun my-anzu-pcre-mode (f prompt beg end use-re overlay-limit)
    (if (and use-re pcre-mode)
        (let ((res (funcall f (concat prompt " (PCRE)") beg end use-re overlay-limit)))
          (condition-case nil
              (rxt-pcre-to-elisp res)
            (error res)))
      (funcall f prompt beg end use-re overlay-limit)))

  (advice-add 'anzu--check-minibuffer-input :around #'my-anzu-wangle-minibuffer-input)
  (advice-add 'anzu--query-from-string :around #'my-anzu-pcre-mode))

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
  :bind ("C-x C-b" . ibuffer))

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

(use-package expand-region
  :defer t
  :ensure t
  :bind
  ("C-=" . er/expand-region))

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

(use-package tabbar
  :ensure t
  :config


  ;; https://emacs.stackexchange.com/questions/984/what-is-the-right-way-to-install-tab-bar


  (defun reset-header-line (&rest args)
    (when tabbar-mode
      (setq-local header-line-format
                  '(:eval (tabbar-line)))
      )
    )

  (advice-add 'notmuch-show--build-buffer :after 'reset-header-line)
  
  (defun my-tabbar-groups ()
    (cond
     ((memql major-mode '(notmuch-show-mode notmuch-search-mode notmuch-message-mode message-mode))
      (list "email"))

     ((memql major-mode '(rcirc-mode))
      (list "IRC"))
     
     ((or (not (projectile-project-p))
          
          (not (or (buffer-file-name)
                   dired-directory
                   (get-buffer-process (current-buffer)))))
      (tabbar-buffer-groups))
     
     (t
      (list (projectile-project-name)))))

  (setq tabbar-buffer-groups-function
        'my-tabbar-groups)
  

  (defun tabbar-disable-bg-color (o &rest args)
    (let ((tabbar-background-color nil))
      (apply o args)))

  (advice-add 'tabbar-background-color :around 'tabbar-disable-bg-color)

  (setq tabbar-separator '(1.0))

  (defun reset-tabbar-mode ()
    (set-face-attribute 'tabbar-default nil
     :inherit 'default
     :background (face-attribute 'mode-line :background nil t)
     :foreground 'unspecified
     :box nil)

    (set-face-attribute 'tabbar-button nil
     :background 'unspecified
     :box nil)

    (set-face-attribute 'tabbar-selected nil
     :weight 'bold
     :foreground 'unspecified
     :background 'unspecified
     :box 1
     :underline t)

    (set-face-attribute 'tabbar-unselected nil
     :box (face-attribute 'shadow :foreground nil t))

    (set-face-attribute 'tabbar-highlight nil
     :foreground 'unspecified :background 'unspecified
     :underline 'unspecified
     :inverse-video t)

    (set-face-attribute 'tabbar-modified nil
     :foreground 'unspecified
     :background 'unspecified
     :box (face-attribute 'shadow :foreground nil t))

    (set-face-attribute 'tabbar-selected-modified nil
     :slant 'italic
     :box 1
     :foreground 'unspecified :background 'unspecified
     :inherit 'tabbar-selected)
    
    (tabbar-mode -1)
    (tabbar-mode 1)
    )
  
  (reset-tabbar-mode)
  (add-hook 'custom-theme-load-hook 'reset-tabbar-mode)
  
  )

(use-package multiple-cursors
  :defer t
  :bind ("C-/" . mc/mark-more-like-this-extended)
  )
