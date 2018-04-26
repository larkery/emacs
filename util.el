(use-package magit
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
              )
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'prog-mode-hook 'show-smartparens-mode)

  :config
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
