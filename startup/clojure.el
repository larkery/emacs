(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)

  :config
  (add-to-list 'display-buffer-alist
        `(,(rx "cider")
          (display-buffer-reuse-window display-buffer-pop-up-window)
          (reusable-frames . nil)))

  (setq cider-mode-line-show-connection nil)

  (defun cider-unaliased-keyword (sym)
    (if (string-match (rx bos "::" (group (+ any)) "/" (group (+ any)) eos) sym)
        (let* ((sym-ns-alias (match-string 1 sym))
               (sym-name (match-string 2 sym))
               (expanded-ns (cider-resolve-alias (cider-current-ns) sym-ns-alias)))
          (format ":%s/%s" expanded-ns sym-name))
      sym))

  (defun cider-eldoc-format-function-with-spec (o thing pos eldoc-info)
    (let ((result (funcall o thing pos eldoc-info))
          (specs (lax-plist-get eldoc-info "specs")))
      (if specs
          (format "%s (has spec)" result)
        result)))


  (defun cider-eldoc-info-add-specs (output)
    (let ((sym (lax-plist-get output "symbol")))

      (if (and sym (string-prefix-p ":" sym))
          (let* ((full-sym (cider-unaliased-keyword sym))
                 (specs (cider-sync-request:spec-list (regexp-quote
                                                       (substring full-sym 1)))))
            (append (list "specs" specs) output))
        output)))

  (advice-add 'cider-eldoc-info :filter-return 'cider-eldoc-info-add-specs)
  (advice-add 'cider-eldoc-format-function :around 'cider-eldoc-format-function-with-spec)
  )

(use-package clj-refactor
  :ensure t
  :commands clj-refactor-mode-on
  :init
  (add-hook 'clojure-mode-hook #'clj-refactor-mode-on)
  :config
  (defun clj-refactor-mode-on ()
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  
  )

(use-package clojure-snippets
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'clojure-mode
    (require 'clojure-snippets)))

