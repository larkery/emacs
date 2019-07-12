(use-package nix-mode
  :ensure t :defer t
  :custom
  (nix-indent-function 'nix-indent-line))

(use-package pdf-tools
  :ensure t
  :defer t
  :commands pdf-view-mode)

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :commands web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-auto-close-style 2))

