(use-package clojure-mode
  :ensure t
  :after major-mode-hydra
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode))
  :config
  (defun clojure-mode-rename-to-ns ()
    (when (buffer-file-name)
      (when (string-match-p (rx  "/core.clj" (? (any "cs")) eos) (buffer-file-name))
       (rename-buffer
        (concat
         (let ((part (split-string (buffer-file-name) "/")))
           (elt part (- (length part) 2))
           )
         "/"
         (file-name-nondirectory (buffer-file-name)))
        t))))
  
  (add-hook 'clojure-mode-hook 'clojure-mode-rename-to-ns))

(use-package cider
  :ensure t
  
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-mode-line-show-connection nil)
  (nrepl-repl-buffer-name-template "*cider-repl %s(%r:%S)*")
  (cider-session-name-template "%j:%H:%p")
  (nrepl-sync-request-timeout 30)
  :config
  (add-to-list 'display-buffer-alist
        `(,(rx "cider")
          (display-buffer-reuse-window display-buffer-pop-up-window)
          (reusable-frames . nil)))

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

  (require 'cider-reflect-doc)
  (require 'cider-fix-sesman)

  (require 'halp)

  (add-hook 'cider-mode-hook (lambda () (setq halp-backend 'cider)))

  (cl-defmethod halp-text ((provider (eql cider)))
    (let ((s (thing-at-point 'symbol)))
      (when s
        (let ((b (cider-create-doc-buffer s)))
          (when b (with-current-buffer b (buffer-string)))))))

  (cl-defmethod halp-open ((provider (eql cider)))
    (let ((s (thing-at-point 'symbol)))
      (when s
        (let ((b (cider-create-doc-buffer s)))
          (when b (pop-to-buffer b))))))

  (define-clojure-indent
    (defroutes 'defun)
    (jdbc.core/atomic 1)
    (>defn 'defun)
    (>defn- 'defun)
    (forM 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)))

(use-package quick-peek
  :ensure t
  :defer t
  :commands quick-peek-show quick-peek-hide)

(use-package clj-refactor
  :ensure t
  :commands clj-refactor-mode
  :after hydra
  :bind (:map clj-refactor-map ("C-c r" . clj-refactor-mode-hydra/body))
  :init
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :config

  (pretty-hydra-define clj-refactor-mode-hydra
    (:quit-key "q")

    ("Refactor"
     (("r" cljr-rename-symbol "rename")
      ("e c" cljr-extract-constant "extract constant")
      ("e d" cljr-extract-def "extract def")
      )

     "Let"
     (("l x" cljr-expand-let "expand")
      ("l l" cljr-introduce-let "introduce")
      ("l r" cljr-remove-let "remove"))
     )))

(use-package clojure-snippets
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'clojure-mode
    (require 'clojure-snippets)))

