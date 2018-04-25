(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(setq default-frame-scroll-bars 'right)
(setq default-frame-alist '((scroll-bar-width . 10)))
(setq-default scroll-bar-width 10)

(use-package ivy
  :ensure t
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :config (counsel-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))
