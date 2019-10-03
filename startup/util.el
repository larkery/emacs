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
  :after (defrepeater)
  :config
  (global-set-key [remap winner-redo] 'winner-redo-repeat)
  (global-set-key [remap winner-undo] 'winner-undo-repeat)
  
  (winner-mode 1)
  (defrepeater #'winner-undo)
  (defrepeater #'winner-redo))


(use-package projectile
  :diminish
  :ensure t
  :demand t
  :custom
  (projectile-keymap-prefix (kbd "C-c p"))
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action 'projectile-dired)
  :config
  (projectile-global-mode 1)
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (bind-keys
   :map projectile-mode-map
   ("<f9>" . projectile-switch-project)
   ("C-c p s s" . counsel-ag)
   ("C-c p s a" . projectile-ag)
   ("M-s p" . projectile-ag)
   ("C-c p d" . projectile-dired)))


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
   ;; ("<f5>" . anzu-query-replace-regexp)

   ;; ("M-s N" . anzu-replace-at-cursor-thing)

   ;; ("M-s n" . anzu-query-replace-at-cursor)

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
         ("C-," . dumb-jump-back))
  :custom
  (dumb-jump-git-grep-cmd "git grep --no-recurse-submodules"))


(use-package which-key
  :diminish ""
  :ensure t
  :custom
  (which-key-idle-delay 0.75)
  :config
  (which-key-mode 1))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name
     try-complete-file-name-partially
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill)))

(use-package auth-source-pass
  :ensure t
  :after auth-source
  :config
  (auth-source-pass-enable))

(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-mode-all))
  :diminish god-local-mode
  :config

  (define-key god-local-mode-map (kbd ".") 'repeat)

  (add-to-list 'god-exempt-major-modes 'notmuch-search-mode)
  (add-to-list 'god-exempt-major-modes 'notmuch-show-mode)

  (lexical-let (cookie)
    (defun god-mode-hl-line ()
      (if god-local-mode
          (setq cookie
                (face-remap-add-relative 'mode-line
                                         '(:background "darkred" :foreground "white")))
        (face-remap-remove-relative cookie))))
    
  (add-hook 'god-local-mode-hook #'god-mode-hl-line)
  
  (defun god-mode-lookup-command (key-string)
    "Execute extended keymaps such as C-c, or if it is a command,
call it."
    (let* ((key-vector (read-kbd-macro key-string t))
           (binding (key-binding key-vector)))
      (cond ((commandp binding)
             (setq last-command-event (aref key-vector (- (length key-vector) 1)))
             binding)
            ((keymapp binding)
             (god-mode-lookup-key-sequence nil key-string))
            
            (:else
             (let* ((end-pos (- (length key-vector) 1))
                    (end-elt (elt key-vector end-pos))
                    (mods (event-modifiers end-elt))
                    (raw (event-basic-type end-elt)))
               (if (equal mods '(control))
                   (progn (aset key-vector end-pos raw)
                          (god-mode-lookup-command
                           (format-kbd-macro key-vector 1)))
                 (error "God: Unknown key binding for `%s`" key-string)))))))

  (with-eval-after-load 'which-key
    (which-key-enable-god-mode-support)))

(use-package editorconfig
  :ensure t
  :diminish
  :custom
  (editorconfig-exclude-regexps
   '("\\`\\(?:ftp\\|https?\\|rsync\\|sftp\\):"
     "\\`/net/"))
    
  :config
  (editorconfig-mode 1))

(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package multiple-cursors
  :defer t
  :bind (("C-/" . mc/mark-more-like-this-extended)
         ("C-M-/" . mc/mark-all-dwim)))

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
  :commands yas/expand
  :init
  
  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)
  (bind-key "M-#" #'yas-expand yas-minor-mode-map)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t)

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

(use-package executable
  :ensure nil
  :hook
  ((after-save . executable-make-buffer-file-executable-if-script-p)))

(defun terminal-here ()
  (interactive)
  (let* ((file-name-at-point (thing-at-point 'filename))
         (directory (cond
                     ((and file-name-at-point
                           (file-exists-p file-name-at-point))
                      (if (file-directory-p file-name-at-point)
                          file-name-at-point
                        (file-name-directory file-name-at-point)))
                     
                     (buffer-file-name
                      (file-name-directory buffer-file-name))
                     
                     ((eq major-mode 'dired-mode)
                      (dired-current-directory))
                     
                     (t default-directory))))
    (when (and directory
               (not (file-remote-p directory)))
      (start-process "" nil "env" "-C" directory "urxvt"))))

(use-package calc
  :defer t
  :bind ("<f8>" . quick-calc)
  :custom (calc-multiplication-has-precedence nil)
  :config
  (defun calc-eval-line ()
    (interactive)
    (save-excursion
      (beginning-of-line)
      (let ((here (point)))
        (search-forward-regexp (rx (| (: (* blank) "=>" (* blank)) eol)) nil t)
        (goto-char (match-beginning 0))
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (insert " => " (calc-eval (buffer-substring here (point))))))
    (end-of-line)))

(use-package dictionary
  :ensure t)

(defun byte-recompile-user-directory ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (shell-command
     "find . -iname \\*.elc -exec rm '{}' ';'"))
  (byte-recompile-directory user-emacs-directory 0 t))

(use-package hydra
  :ensure t)

(use-package pretty-hydra :ensure t
  :bind ("<f1>" . general-keys/body)
  :config

  (pretty-hydra-define general-keys
    
    (:foreign-keys warn
                   :quit-key "q" :title "Useful keys"
                   :exit t)

    ("Search"
     (("r r" anzu-query-replace-regexp "rep reg")
      ("r s" anzu-query-replace "rep str"))
     "Edit"
     (("i n" insert-numbers "ins num")
      ("i f" insert-file-path "ins path")
      ("e l" calc-eval-line "calc line"))
     "Set"
     (("d e" toggle-debug-on-error "debg err")
      ("d q" toggle-debug-on-quit "debg q")
      ("h l" hl-line-mode "hl-line"))
     "Win"
     (("," winner-undo :exit nil)
      ("." winner-redo :exit nil))))

  (defun insert-numbers ()
    (interactive)

    (cond
     ((> (mc/num-cursors) 1)
      (call-interactively #'mc/insert-numbers))

     (t (call-interactively #'kmacro-insert-counter))))
  
  )

(use-package major-mode-hydra :ensure t
  :bind ("<f2>" . major-mode-hydra)
  :commands major-mode-hydra major-mode-hydra-define)

(use-package diff-hl :ensure t
  :custom (diff-hl-side 'right))

(use-package scf-mode
  :commands scf-mode
  :bind (:map grep-mode-map
              (")" . scf-mode)))

