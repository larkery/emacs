(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.cljs\\'" . clojurescript-mode)))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in))
