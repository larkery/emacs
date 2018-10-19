(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style (quote european))
 '(custom-safe-themes
   (quote
    ("3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" default)))
 '(debug-on-error nil)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#452E2E")
 '(package-selected-packages
   (quote
    (shift-number ox-reveal htmlize date-at-point lsp-ui lsp-java visual-regexp-steroids comment-dwim-2 clojure-snippets clj-refactor composable easy-kill ace-popup-menu gruvbox-theme symbol-overlay dired-subtree transpose-frame haskell-mode rainbow-mode wgrep multiple-cursors yascroll yaml-mode xmlgen which-key use-package tabbar syntactic-close srcery-theme smartparens rcirc-notify projectile powerline pcre2el num3-mode nordless-theme nix-mode mode-icons magit god-mode expand-region editorconfig dumb-jump diminish counsel color-theme-sanityinc-tomorrow cider browse-kill-ring birds-of-paradise-plus-theme base16-theme autothemer auth-source-pass anzu ag adaptive-wrap)))
 '(recentf-auto-cleanup 300)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (lambda nil
             (shell-command "pandoc -f org -t docbook changelog.org --top-level-division=chapter | sed 's! id=\"\"!!g' | sed 's!<chapter.*>!<chapter xmlns=\"http://docbook.org/ns/docbook\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">!g' | sed 's!<literal>\\(ref\\..\\+\\)</literal>!<xref linkend=\"\\1\"/>!g' > changelog.xml"))
           nil t)
     (eval add-hook
           (quote after-save-hook)
           (lambda nil
             (shell-command "pandoc -f org -t docbook changelog.org --top-level-division=chapter | sed 's! id=\"\"!!g' | sed 's!<chapter.*>!<chapter xmlns=\"http://docbook.org/ns/docbook\" xmlns:xlink=" http://www\.w3\.org/1999/xlink ">!g' | sed 's!<literal>\\(ref\\..\\+\\)</literal>!<xref linkend=\"\\1\"/>!g' > changelog.xml"))
           nil t)
     (eval add-hook
           (quote after-save-hook)
           (lambda nil
             (shell-command "pandoc -f org -t docbook changelog.org --top-level-division=chapter | sed 's! id=\"\"!!g' | sed 's!<chapter.*>!<chapter xmlns=\"http://docbook.org/ns/docbook\">!g' | sed 's!<literal>\\(ref\\..\\+\\)</literal>!<xref linkend=\"\\1\"/>!g' > changelog.xml"))
           nil t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date-weekend ((t (:inherit (org-agenda-date italic) :weight bold)))))
