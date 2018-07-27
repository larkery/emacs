(deftheme tweaks)

(custom-theme-set-faces
 'tweaks
 '(org-todo ((default (:weight bold))))
 '(dired-subtree-depth-1-face
   ((((background light))
     (:background "grey90"))
    (((background dark))
     (:background "grey10"))))
 '(dired-subtree-depth-2-face
   ((((background light))
     (:background "grey85"))
    (((background dark))
     (:background "grey15"))))
 '(dired-subtree-depth-3-face
   ((((background light))
     (:background "grey90"))
    (((background dark))
     (:background "grey10"))))
 '(dired-subtree-depth-4-face
   ((((background light))
     (:background "grey85"))
    (((background dark))
     (:background "grey15"))))
  '(dired-subtree-depth-5-face
   ((((background light))
     (:background "grey90"))
    (((background dark))
     (:background "grey10"))))
 '(dired-subtree-depth-6-face
   ((((background light))
     (:background "grey85"))
    (((background dark))
     (:background "grey15"))))

 '(symbol-overlay-default-face
   ((t (:underline "grey50")))
   )
 )

(provide-theme 'tweaks)
