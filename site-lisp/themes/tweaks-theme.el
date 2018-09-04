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

 '(notmuch-search-unread-face ((t (:foreground "white" :distant-foreground "black" :weight bold))))
 '(symbol-overlay-default-face ((t (:underline "grey50"))))
 )

(provide-theme 'tweaks)
