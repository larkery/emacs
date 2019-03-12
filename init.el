(setq messages-buffer-max-lines 100000)
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(require 'use-package-save-custom)
(setq gc-cons-threshold 16000000)

(setq gc-cons-threshold (* 100 gc-cons-threshold)
      make-backup-files nil
      custom-file (concat user-emacs-directory "custom.el"))

(load custom-file)

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
;;	     '("melpa" . "https://melpa.org/packages/")
             '("melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/")
             )
(add-to-list 'package-archives
	     '("org-mode" . "https://orgmode.org/elpa/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(let ((startup-directory (concat user-emacs-directory "startup/")))
  (load (concat startup-directory "basics.el"))
  (load (concat startup-directory "dired.el"))
  (load (concat startup-directory "mode-line.el"))
  (load (concat startup-directory "email.el"))
  (load (concat startup-directory "util.el"))
  (load (concat startup-directory "clojure.el"))
  (load (concat startup-directory "modes.el"))
  (load (concat startup-directory "org.el")))

(setq gc-cons-threshold (/ gc-cons-threshold 100))
