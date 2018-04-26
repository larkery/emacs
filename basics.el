(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(set-fringe-mode 4)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)

(setq default-frame-scroll-bars 'right
      default-frame-alist '((scroll-bar-width . 10))
      focus-follows-mouse t
      mouse-autoselect-window t
      echo-keystrokes 0.2
      disabled-command-function nil
      )

(setq-default scroll-bar-width 10
	      indent-tabs-mode nil
	      case-fold-search t
	      bidi-display-reordering nil
	      indicate-empty-lines t
	      indicate-buffer-boundaries 'right)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f5>") 'save-buffer)

(global-auto-revert-mode 1)

(use-package ivy
  :ensure t
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :config (counsel-mode)
  (setq counsel-find-file-ignore-regexp "\\`\\."))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package paren
  :config
  (show-paren-mode 1))
