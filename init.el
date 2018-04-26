(setq gc-cons-threshold (* 100 gc-cons-threshold)
      make-backup-files nil
      custom-file (concat user-emacs-directory "custom.el"))

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(load (concat user-emacs-directory "basics.el"))
(load (concat user-emacs-directory "email.el"))
(load (concat user-emacs-directory "util.el"))
(load (concat user-emacs-directory "clojure.el"))
(load custom-file)

(setq gc-cons-threshold (/ gc-cons-threshold 100))
