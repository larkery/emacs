(use-package magit
  :bind ("C-c g" . magit-status)
  :ensure t
  :commands magit-status)

(use-package vc
  :defer t
  :config
  (add-hook 'vc-annotate-mode-hook 'vc-annotate-toggle-annotation-visibility))

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
              ("C-&" . sp-convolute-sexp)
              ("C-~" . sp-rewrap-cycle)
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

(use-package project
  ;; :bind
  ;; (;; ("C-c f" . project-find-file)
  ;;  ;; ("C-c d" . project-dired)
  ;;  ;; ("M-s p" . project-find-regexp)

  ;;  ;; ("C-c k" . project-kill)
  ;;  )

  
  :config
  ;; (defun project-dired (&optional prefix)
  ;;   (interactive "P")
  ;;   (if-let ((cur (and (not prefix)
  ;;                      (project-current nil))))
  ;;       (dired (car (project-roots cur)))
  ;;     (counsel-bookmarked-directory)))

  
  ;; (defun project-kill ()
  ;;   (interactive)
  ;;   (when (y-or-n-p "Kill project?")
  ;;     (when-let ((cur (project-current nil)))
  ;;       (cl-loop
  ;;        for b being the buffers
  ;;        when (with-current-buffer b (equal (project-current nil) cur))
  ;;        do (kill-buffer b)))))


  (defun project-current-root ()
    (when-let ((cur (project-current nil))) (directory-file-name (cdr cur))))

  (defun project-current-name ()
    (when-let ((root (project-current-root))) (file-name-nondirectory root)))
  )

(use-package pcre2el
  :diminish pcre-mode
  :ensure t
  :config
  (pcre-mode t)

  (with-eval-after-load 'xref
    (advice-add 'xref--regexp-to-extended :override #'rxt-elisp-to-pcre)))

(use-package replace
  :bind ("M-s o" . occur)
  :defer t
  :commands occur)

(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . 'anzu-query-replace)
   ([remap query-replace-regexp] . 'anzu-query-replace-regexp)

   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  
  :defer t
  :commands (anzu-query-replace
             anzu-query-replace-at-cursor
             anzu-isearch-query-replace)
  :config

  (defun my-anzu-pcre-to-elisp (query)
    (let* ((sep (anzu--separator))
           (to (when (and (string-match sep query)
                          (get-text-property (match-beginning 0) 'separator query))
                 (substring-no-properties query (match-end 0))))
           (from (or (and to (substring-no-properties query 0 (match-beginning 0)))
                     query))
           (from-el (rxt-pcre-to-elisp from)))
      (concat from-el
              (when to sep)
              to)))
    
  (defun my-anzu-wangle-minibuffer-input (f buf beg end use-re overlay-limit)
    (if (and use-re pcre-mode)
        (let ((-minibuffer-contents (symbol-function 'minibuffer-contents)))
          (cl-letf (((symbol-function 'minibuffer-contents)
                     (lambda ()
                       (condition-case nil
                           (my-anzu-pcre-to-elisp (funcall -minibuffer-contents))
                         (error ""))
                       )))
            
            (funcall f buf beg end use-re overlay-limit)))
      (funcall f buf beg end use-re overlay-limit)))

  (defun my-anzu-pcre-mode (f prompt beg end use-re overlay-limit)
    (if (and use-re pcre-mode)
        (cl-letf (((symbol-function 'anzu--transform-from-to-history)
                   (lambda ()
                     (let ((separator (anzu--separator)))
                       (append (mapcar (lambda (from-to)
                                         (concat (query-replace-descr
                                                  (condition-case nil
                                                      (rxt-elisp-to-pcre (car from-to))
                                                    (error (car from-to))))
                                                 separator
                                                 (query-replace-descr (cdr from-to))))
                                       anzu--query-defaults)
                               (mapcar
                                (lambda (r)
                                  (condition-case nil
                                      (rxt-elisp-to-pcre r)
                                    (error r)))
                                (symbol-value query-replace-from-history-variable))))
                     )
                   )
                  ((symbol-function 'anzu--validate-regexp)
                   (lambda (q) t)))
          (my-anzu-pcre-to-elisp (funcall f (concat prompt " (PCRE)") beg end use-re overlay-limit))
          ;; (condition-case nil
          
          ;;   (error (error "Invalid regexp")))
          )
      (funcall f prompt beg end use-re overlay-limit)))

  (advice-add 'anzu--check-minibuffer-input :around #'my-anzu-wangle-minibuffer-input)
  (advice-add 'anzu--query-from-string :around #'my-anzu-pcre-mode)

  ;; (advice-remove 'anzu--check-minibuffer-input #'my-anzu-wangle-minibuffer-input)
  ;; (advice-remove 'anzu--query-from-string #'my-anzu-pcre-mode)
  
  (setq anzu-mode-lighter ""))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-git-grep-cmd "git grep --no-recurse-submodules")
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     
     try-complete-file-name-partially
     try-complete-file-name
     )))

(use-package auth-source-pass
  :ensure t
  :after auth-source
  :config
  (auth-source-pass-enable))

(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package multiple-cursors
  :defer t
  :bind
  (("C-/" . mc/mark-more-like-this-extended)
   ("C-M-/" . mc/mark-all-dwim)
   :map mc/keymap
   ("C-c SPC" . mc/vertical-align-with-space)
   ("C-c n" . mc/insert-numbers)
   ("C-c l" . mc/insert-letters))
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
  :custom
  (symbol-overlay-idle-time 0.75)
  :init
  (add-hook 'prog-mode-hook 'symbol-overlay-mode))

(use-package browse-url
  :custom
  (browse-url-generic-program "xdg-open")
  (browse-url-browser-function 'browse-url-generic))

(defun insert-file-path ()
  (interactive)
  (push-mark)
  (let ((path (read-file-name "Insert path: ")))
    (when path
      (when (derived-mode-p 'prog-mode)
        (insert "\""))
      (insert path)
      (when (derived-mode-p 'prog-mode)
        (insert "\"")))))

(use-package yasnippet
  :ensure t
  :defer nil
  :diminish yas-minor-mode
  :commands yas/expand
  :init
  
  :config
  (require 'yasnippet-snippets)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-reload-all)
  (bind-key "C-<return>" #'yas-expand yas-minor-mode-map)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; (use-package goto-chg
;;   :ensure t
;;   :bind ("C-=" . goto-last-change))


(use-package executable
  :ensure nil
  :hook
  ((after-save . executable-make-buffer-file-executable-if-script-p)))

(use-package calc
  :defer t
  :bind ("<f8>" . quick-calc)
  :custom
  (calc-multiplication-has-precedence nil)
  :config
  ;; (require 'calc-hydras)
  
  (with-eval-after-load 'calc
    (defalias 'calcFunc-uconv 'math-convert-units)))

(use-package dictionary :ensure t
  :bind ("C-c ?" . dictionary-search)
  :custom
  (dictionary-server "dict.org"))

(defun byte-recompile-user-directory ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (shell-command
     "find . -iname \\*.elc -exec rm '{}' ';'"))
  (byte-recompile-directory user-emacs-directory 0 t))

;; (use-package hydra :ensure t)

;; (use-package pretty-hydra :ensure t)

;; (use-package major-mode-hydra :ensure t
;;   :bind ("<f1>" . major-mode-hydra)
;;   :commands major-mode-hydra major-mode-hydra-define)

;; (use-package diff-hl :ensure t
;;   :custom (diff-hl-side 'right))

(use-package scf-mode
  :commands scf-mode
  :bind (:map grep-mode-map (")" . scf-mode)))

;; (use-package macro-math
;;   :bind (("C-c e" . macro-math-eval-region)))


(use-package literate-calc-mode
  :ensure t
  :bind (("C-c =" . literate-calc-minor-mode))
  :config
  
  (defvar literate-calc--result
    (rx bol
        (opt (1+ (or letter blank)))
        "="
        (+? (not (any ?=)))
        (group (* blank) "=> " (1+ any))
        eol))
  
  (defun literate-calc-remove-results ()
    (interactive)
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (search-forward-regexp literate-calc--result nil t)
        (goto-char (match-beginning 1))
        (kill-line))))

  (defun literate-calc-toggle ()
    (if literate-calc-minor-mode
        (literate-calc-remove-results)
      (when (y-or-n-p "Keep results?")
        (literate-calc-insert-results))))
  
  (add-hook 'literate-calc-minor-mode-hook
            'literate-calc-toggle))

(use-package halp
  :bind ("C-?" . halp-inline)
  )

(use-package minibuffer :config
  ;; this is to make filenames always completeable with C-M-i
  
  (autoload 'ffap-file-at-point "ffap")
  (defun complete-path-at-point+ ()
    "Return completion data for UNIX path at point."
    (let ((fn (ffap-file-at-point))
          (fap (thing-at-point 'filename)))
      (when (and (or fn (equal "/" fap))
                 (save-excursion
                   (search-backward fap (line-beginning-position) t)))
        (list (match-beginning 0)
              (match-end 0)
              #'completion-file-name-table :exclusive 'no))))

  (add-hook 'completion-at-point-functions
            #'complete-path-at-point+
            'append))

;; (use-package rainbow-mode
;;   :commands rainbow-mode
;;   :config

;;   (put 'color 'bounds-of-thing-at-point
;;        'color-bounds-at-point)

;;   (defun color-bounds-at-point ()
;;     (save-excursion
;;       (skip-chars-backward "#0123456789abcdefABCDEF")
;;       (if (looking-at (rx "#" (| (= 6 hex) (= 3 hex))))
;;           (cons (point) (match-end 0))
;;         nil)))

;;   (defun color-adjust-at-point (component amount)
;;     (when-let ((bounds (bounds-of-thing-at-point 'color)))
;;       (replace-region-contents
;;        (car bounds) (cdr bounds)
;;        (lambda ()
;;          (let* ((amount (/ (or amount 5) 100.0))
;;                 (color (color-name-to-rgb (buffer-string)))
;;                 (color (apply 'color-rgb-to-hsl color))
;;                 (bounds (bounds-of-thing-at-point 'color)))

;;            (setf (nth component color)
;;                  (min 1.0 (max 0.0 (+ amount (nth component color)))))
;;            (apply 'color-rgb-to-hex (nconc (apply 'color-hsl-to-rgb color)
;;                                            '(2))))))))

;;   (define-minor-mode rainbow-colors-and-keys-mode
;;     "rainbow mode + keybindings"
;;     :keymap
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "C-c c l") 'color-lighten-at-point)
;;       (define-key map (kbd "C-c c s") 'color-saturate-at-point)
;;       (define-key map (kbd "C-c c h") 'color-rotate-at-point)
;;       map))
  
;;   (defun color-lighten-at-point (prefix)
;;     (interactive "P")
;;     (color-adjust-at-point 2 prefix))
  
;;   (defun color-saturate-at-point (prefix)
;;     (interactive "P")
;;     (color-adjust-at-point 1 prefix))
  
;;   (defun color-rotate-at-point (prefix)
;;     (interactive "P")
;;     (color-adjust-at-point 0 prefix))
  
;;   )

(use-package eldoc :diminish eldoc-mode)

(use-package saveplace :config (save-place-mode))

(use-package autoinsert
  :custom
  (auto-insert-query nil)
  (auto-insert-alist
   `((,(rx "/shell.nix" eos) . ["shell.nix" autoinsert-yas-expand]))
   
   )
  
  :config

  (auto-insert-mode t)
  
  (setq auto-insert-directory (concat user-emacs-directory "template"))
  (defun autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas/expand-snippet (buffer-string) (point-min) (point-max)))
  )

(defun browse-url-at-or-after-point ()
  (interactive)

  (if (thing-at-point 'url)
      (browse-url-at-point)
    
      )
  )


(defun tramp-cleanup-connections-harder ()
  (interactive)
  (cl-loop
   for b being the buffers
   when (with-current-buffer b
          (and (not buffer-file-name)
               (not (string-match-p (rx bos "*tramp/") (buffer-name)))
               (tramp-tramp-file-p default-directory)))
   do (with-current-buffer b (setq default-directory "~/"))))

(defun calc-eval-line-or-region ()
  (interactive)
  (if (region-active-p)
      (let ((e (calc-eval
                (buffer-substring-no-properties
                 (mark)
                 (point)))))
        (delete-region (mark) (point))
        (insert e))
    
      (save-excursion
        (beginning-of-line)
        (if (re-search-forward " *=.*" (line-end-position) t)
            (replace-match ""))
        (end-of-line)
        (insert " = "
                (calc-eval
                 (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)
                  ))))))


(bind-key "C-=" #'calc-eval-line-or-region)

