(deftheme tweaks)

(custom-theme-set-faces
 'tweaks
 '(org-todo ((default (:weight bold))))
 '(dired-subtree-depth-1-face ((t (:slant italic))))
 '(dired-subtree-depth-2-face ((t (:slant nil))))
 '(dired-subtree-depth-3-face ((t)))
 '(dired-subtree-depth-4-face ((t)))
 '(dired-subtree-depth-5-face ((t)))
 '(dired-subtree-depth-6-face ((t)))

 '(notmuch-search-unread-face ((t (:foreground "white" :distant-foreground "black" :weight bold))))
 )

(provide-theme 'tweaks)
