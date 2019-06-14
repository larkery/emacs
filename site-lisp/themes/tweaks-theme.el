(deftheme tweaks)

(custom-theme-set-faces
 'tweaks
 '(org-todo ((default (:weight bold))))
 '(dired-subtree-depth-1-face ((t (:slant italic))))
 '(dired-subtree-depth-2-face ((t (:slant normal))))
 '(dired-subtree-depth-3-face ((t (:slant italic))))
 '(dired-subtree-depth-4-face ((t (:slant normal))))
 '(dired-subtree-depth-5-face ((t (:slant italic))))
 '(dired-subtree-depth-6-face ((t (:slant normal))))
 '(notmuch-message-summary-face ((t (:inherit hl-line))))

 '(line-spacing ((t nil)))

 '(notmuch-search-unread-face ((t (:box nil :foreground "white" :distant-foreground "black" :weight bold))))

 '(notmuch-search-flagged-face ((t (:foreground "green4" :distant-foreground "cyan4" :weight bold))))
 
 '(symbol-overlay-default-face ((t (:underline "grey50"))))

 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "yellow"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "lightblue"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "white"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-depth-1-face :foreground nil))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-depth-2-face :foreground nil))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-depth-3-face :foreground nil))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-depth-1-face :foreground nil))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-depth-2-face :foreground nil))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-depth-3-face :foreground nil))))

 '(rainbow-delimiters-unmatched-face ((t (:background "red" :foreground "white"))))
 '(rainbow-delimiters-mismatched-face ((t (:background "red" :foreground "white"))))

 '(diredfl-dir-heading ((t (:inherit match))))
 '(diredfl-read-priv ((t (:inherit default))))
 '(diredfl-write-priv ((t (:inherit default))))
 '(diredfl-exec-priv ((t (:inherit default))))
 '(diredfl-no-priv ((t (:inherit default))))
 '(diredfl-ignored-file-name ((t (:inherit dired-ignored))))
 '(diredfl-number ((t (:inherit default))))
 '(diredfl-file-name ((t (:inherit default))))
 '(diredfl-date-time ((t (:inherit bold))))
 '(diredfl-symlink ((t (:inherit link))))
 '(diredfl-file-suffix ((t (:inherit bold))))
 '(diredfl-dir-name ((t (:inherit (dired-directory bold)))))
 '(diredfl-compressed-file-name ((t (:inherit default))))
 '(diredfl-compressed-file-suffix ((t (:inherit warning))))
 '(diredfl-deletion ((t (:inherit error))))
 '(ag-match-face ((t (:inherit match))))
 '(mode-line ((t (:overline t :underline nil :box nil))))
 '(mode-line-inactive ((t (:overline t :underline nil :box nil))))
 '(web-mode-json-key-face ((t (:inherit font-lock-variable-name-face))))
 )


(provide-theme 'tweaks)
