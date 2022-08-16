(use-package flyspell
  :config
  (cond
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"
          ispell-local-dictionary "en_GB-ise"
          ispell-local-dictionary-alist
          '(("en_GB-ise" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB-ise") nil utf-8)))
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))
  
  
  )
