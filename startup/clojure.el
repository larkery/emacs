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

  (defun cider-short-mode-line ()
    (if-let* ((current-connection (ignore-errors (cider-current-connection))))
      (with-current-buffer current-connection
        (concat
         cider-repl-type
         (when cider-mode-line-show-connection
           (format ":%s%s"
                   (pcase (car nrepl-endpoint)
                     ("localhost" "")
                     (x (concat ":@" x)))
                   (cadr nrepl-endpoint))
           )))
      "none"))
  
  (setq cider-mode-line
        '(:eval (concat " " (cider-short-mode-line))))
  
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

