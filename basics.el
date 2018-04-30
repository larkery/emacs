(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(set-fringe-mode 4)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)
(window-divider-mode 1)

(setq default-frame-scroll-bars nil
      default-frame-alist '((scroll-bar-width . 8)
                            (right-divider-width . 4))
      focus-follows-mouse t
      mouse-autoselect-window t
      echo-keystrokes 0.2
      disabled-command-function nil
      set-mark-command-repeat-pop t
      )

(setq-default scroll-bar-width 8
	      indent-tabs-mode nil
	      case-fold-search t
	      bidi-display-reordering nil
	      indicate-empty-lines t
	      indicate-buffer-boundaries 'right)

(advice-add 'set-mouse-position :override #'ignore)
(advice-add 'set-mouse-pixel-position :override #'ignore)
(advice-add 'set-mouse-absolute-pixel-position :override #'ignore)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f5>") 'save-buffer)

(global-auto-revert-mode 1)

(use-package diminish
  :ensure t)

(use-package ivy
  :diminish
  :ensure t
  :defer nil
  :bind ("M-s M-s" . swiper-at-point)
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (or (thing-at-point 'symbol)
                (thing-at-point 'word)))))

(use-package counsel
  :diminish
  :ensure t
  :config (counsel-mode)
  (setq counsel-find-file-ignore-regexp "\\`\\."))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

(use-package dakrone-light-theme
  :ensure t
  :config
  (load-theme 'dakrone-light t))

(use-package theme-to-xresources
  :config
  (theme-to-xresources))
