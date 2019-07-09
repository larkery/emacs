(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-mode-line-show-connection nil)
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

  (require 'quick-peek)

  (defun cider-doc-quick-peek ()
    (interactive)

    (let ((s (thing-at-point 'symbol)))
      (when s
        (let ((b (cider-create-doc-buffer s)))
          (when b
            (let ((bc (with-current-buffer b (buffer-string))))
              (quick-peek-show bc)
              (set-transient-map nil nil #'quick-peek-hide)
              ))))))

  (bind-key "C-c d" #'cider-doc-quick-peek cider-mode-map)

  (bind-key (concat leader-key* " e ;")
            #'cider-pprint-eval-last-sexp-to-comment
            cider-mode-map)

  (bind-key (leader-kbd "e c")
            #'cider-connect-clj&cljs
            cider-mode-map)
  
  (define-clojure-indent
    (defroutes 'defun)
    (jdbc.core/atomic 1)
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

