(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq messages-buffer-max-lines 100000)
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))

(defvar state-directory (concat user-emacs-directory "state/"))
(defvar conf-directory  (concat user-emacs-directory "conf/"))

(setq gc-cons-threshold most-positive-fixnum
      make-backup-files nil

      custom-file (concat conf-directory "custom.el")
      auto-save-list-file-prefix (concat state-directory "auto-save-list/.saves-")
      
      load-prefer-newer t)

(load custom-file)
(require 'use-package-filter-custom)

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

(add-to-list
 'package-archives
 '("org-mode" . "https://orgmode.org/elpa/"))

(package-initialize)

(setq package-native-compile t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t))

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(let ((init-files (directory-files (concat user-emacs-directory "/init") t "\\.el$")))
  (while init-files
    (load (car init-files))
    (setq init-files (cdr init-files))))

(setq gc-cons-threshold 800000)
