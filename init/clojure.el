(use-package clojure-mode
  :ensure t
  :after major-mode-hydra
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode))
  :bind (:map clojure-mode-map
              ("M-g t" . clojure-find-test-or-impl))
  ;; (bind-key "M-g t" 'clojure-find-test-or-impl clojure-mode-map)
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
  
  (add-hook 'clojure-mode-hook 'clojure-mode-rename-to-ns)


  (defun clojure-find-test-or-impl ()
    (interactive)
    (if-let ((pr (or (project-current-root)
                     (locate-dominating-file
                      (buffer-file-name) "deps.edn"))))
        (let* ((relpath (file-relative-name (buffer-file-name) pr))
               (newpath (if (string-match-p (rx bos "test/") relpath)
                            (replace-regexp-in-string
                             (rx "_test" (group ".clj" (? (any "sc"))) eos) "\\1"
                             (replace-regexp-in-string (rx bos "test/") "src/" relpath))

                          (replace-regexp-in-string
                           (rx (group ".clj" (? (any "sc"))) eos) "_test\\1"
                           (replace-regexp-in-string (rx bos "src/") "test/" relpath))))
               (newpath (concat pr "/" newpath)))
          (find-file newpath)
          (unless (file-exists-p newpath)
            (clojure-insert-ns-form)))
      
      (error "Can't find project root")))

  )
 (use-package cider
  :ensure t

  :bind
  (:map cider-mode-map
        ("C-c C-;" . cider-pprint-eval-last-sexp-to-comment))
  
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-mode-line-show-connection nil)
  (cider-repl-display-help-banner nil)
  (nrepl-repl-buffer-name-template "*cider-repl %s(%r:%S)*")
  (cider-session-name-template "%j:%H:%p")
  (nrepl-sync-request-timeout 30)
  (cider-repl-buffer-size-limit 100000)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-auto-select-error-buffer t)
  (cider-show-error-buffer t)
  (cider-debug-display-locals nil)
  (cider-debug-prompt 'overlay)
  (cider-eval-spinner-type 'progress-bar)
  :config

  (require 'cider-reflect-doc)
  (require 'cider-fix-sesman)

  (require 'halp)

  (add-hook 'cider-disconnected-hook
            (lambda () (rename-buffer
                        (concat "*"
                                "[DEAD] "
                                (substring (buffer-name) 1))
                        t)))
  
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
    (context 2))


  (add-to-list 'paredit-space-for-delimiter-predicates
               (lambda (_ d)
                 (not
                  (and (or (eq major-mode 'clojure-mode)
                           (eq major-mode 'clojurescript-mode))
                       (= d ?{)
                       (= (char-before ?#))))))

  
  )


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

  ;; (pretty-hydra-define clj-refactor-mode-hydra
  ;;   (:quit-key "q")

  ;;   ("Refactor"
  ;;    (("r" cljr-rename-symbol "rename")
  ;;     ("e c" cljr-extract-constant "extract constant")
  ;;     ("e d" cljr-extract-def "extract def")
  ;;     )

  ;;    "Let"
  ;;    (("l x" cljr-expand-let "expand")
  ;;     ("l l" cljr-introduce-let "introduce")
  ;;     ("l r" cljr-remove-let "remove"))
  ;;    ))
  )

(use-package clojure-snippets
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'clojure-mode
    (require 'clojure-snippets)))

