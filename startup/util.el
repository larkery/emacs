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
              ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-|" . sp-split-sexp) ;; misc
              ("C-M-;" . sp-comment-or-uncomment-sexp)
              )
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'show-smartparens-mode)

  :config
  (defun sp-comment-or-uncomment-sexp ()
    (interactive)
    (comment-or-uncomment-region
     (point)
     (save-mark-and-excursion
      (sp-forward-sexp)
      (unless (looking-at ".") (insert "\n"))
      (point))))
  
  (require 'smartparens-config)
  (setq-default sp-autoskip-closing-pair 'always))

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
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'ivy))

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
  (advice-add 'anzu--query-from-string :around #'my-anzu-pcre-mode)
  )

(use-package dumb-jump
  :ensure t
  :defer t
  :bind (("M-." . dumb-jump-go) ;; Go to Symbol, ish
         )
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))
