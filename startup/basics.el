(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(set-fringe-mode '(0 . 8))
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)

(setq window-divider-default-places t 
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      default-frame-scroll-bars nil
      default-frame-alist '((scroll-bar-width . 1)
                            (right-divider-width . 3))
      focus-follows-mouse t
      mouse-autoselect-window t
      echo-keystrokes 0.2
      disabled-command-function nil
      set-mark-command-repeat-pop t

      kill-ring-max 1000

      sentence-end-double-space nil

      calendar-week-start-day 1

      calendar-today-marker 'error
      calendar-offset -1
      calendar-intermonth-spacing 2

      calendar-latitude 51.455313
      calendar-longitude -2.591902

      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-buffer-choice t
      initial-major-mode 'text-mode

      enable-recursive-minibuffers t

      scroll-step 10

      auto-hscroll-mode 'current-line
      mouse-drag-and-drop-region t

      locate-dominating-stop-dir-regexp
      (purecopy (rx bos "/net/" (* (not (any "/"))) "/" (* (not (any "/"))) eos)))

(window-divider-mode 1)

(custom-set-variables '(calendar-date-style 'european))

(setq-default scroll-bar-width 8
	      indent-tabs-mode nil
	      case-fold-search t
	      bidi-display-reordering nil
	      indicate-empty-lines t
	      indicate-buffer-boundaries 'right)

(advice-add 'set-mouse-position :override #'ignore)
(advice-add 'set-mouse-pixel-position :override #'ignore)
(advice-add 'set-mouse-absolute-pixel-position :override #'ignore)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-set-key (kbd "C-z") 'undo)
(defun kill-current-buffer ()
  (interactive) (kill-buffer nil))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-*") 'query-replace-regexp)
(global-set-key (kbd "C-*") 'query-replace)

(defun cycle-space ()
  (interactive) (cycle-spacing -1 t))
(global-set-key [remap just-one-space] 'cycle-space)

(global-auto-revert-mode 1)

(use-package diminish
  :ensure t)

(use-package ivy
  :diminish
  :ensure t
  :defer nil
  :bind (("M-s M-s" . swiper-at-point))
  :custom
  (ivy-use-virtual-buffers t)
  :config (ivy-mode)

  (with-eval-after-load
      'swiper 
    (bind-key "C-'" nil swiper-map))
  
  (defun swiper-at-point ()
    (interactive)
    (swiper (when (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end)))))
            
  (defun ivy-switch-buffer-transformer (str)
    (if (< (window-width (minibuffer-window)) 75) str
      (let* ((b (get-buffer str))
             (typ (if b
                      (with-current-buffer b
                        (cond (dired-directory "/")
                              ((file-remote-p (or buffer-file-name default-directory)) "@")
                              (buffer-file-name "f")
                              (t "%")))
                    "v"))
             (pname (or (and b
                             (with-current-buffer b
                               (let ((fn (or buffer-file-name dired-directory)))
                                 (and fn
                                      (not (file-remote-p fn))
                                      (projectile-project-name)))))
                        "-")))

        (format
         "%s %10s  %s"
         (propertize typ 'face 'error)
         (propertize pname 'face 'shadow) str)))))

(use-package recentf
  :config
  (recentf-mode 1)

  (defun recentf-excluded-deleted-local-files (fi)
    (unless (file-remote-p fi)
      (not (file-exists-p fi))))
  (run-with-idle-timer 600 t 'recentf-save-list)

  :custom
  (recentf-exclude
   (quote
   ("^/home/hinton/notes/" "^/nix/store" recentf-exclude-deleted-local-files)))
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 300))

(use-package counsel
  :diminish
  :defer nil
  :ensure t
  :bind (("C-x C-r" . counsel-recentf)
         :map counsel-mode-map
         ([remap yank-pop] . nil))
  :config
  (counsel-mode 1)
  :custom
  (counsel-find-file-ignore-regexp "\\`\\."))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style (quote forward) nil (uniquify)))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1))

(defvar custom-theme-load-hook nil)
(defadvice load-theme (after update-xresources-after-load-theme activate)
  (run-hooks 'custom-theme-load-hook))

(defun run-custom-theme-load-hook-once (frame)
  (when (window-system frame)
    (run-hooks 'custom-theme-load-hook)
    (remove-hook 'after-make-frame-functions 'run-custom-theme-load-hook-once)))

(add-hook 'after-make-frame-functions 'run-custom-theme-load-hook-once)

(use-package theme-to-xresources
  :defer nil
  :commands theme-to-xresources
  :init
  (add-hook 'custom-theme-load-hook
            (lambda ()
              (theme-to-xresources)
              (set-fringe-mode '(0 . 8))
              (call-process "i3" nil nil nil "reload"))))

(defvar dark-theme 'wombat)
(defvar light-theme 'adwaita)

(add-to-list 'custom-theme-load-path
               (concat user-emacs-directory "site-lisp/themes"))

(unless (package-installed-p 'spacemacs-theme)
  (package-install 'spacemacs-theme t))

(setq dark-theme 'spacemacs-dark)
(setq light-theme 'spacemacs-light)
(setq spacemacs-theme-org-height nil
      spacemacs-theme-org-bold nil)

(load-theme light-theme t)
(load-theme 'tweaks t)

(defun switch-theme ()
  (interactive)
  (let ((themes (if (custom-theme-enabled-p light-theme)
                    (cons light-theme dark-theme)
                  (cons dark-theme light-theme))))
    (disable-theme (car themes))
    (load-theme (cdr themes) t))
  (load-theme 'tweaks t))

(bind-key "<f6>" 'switch-theme)

(setq-default line-spacing nil)

(use-package visual-line
  :commands visual-line-mode
  :init
  (add-hook 'text-mode-hook 'visual-line-mode)
  :custom
  (visual-line-fringe-indicators '(nil right-curly-arrow)))

(use-package browse-kill-ring
  :commands browse-kill-ring
  :ensure t
  :init
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; pinched from browse-kill-ring, so we don't have to autoload it at startup
    (interactive "p")
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))
  (ad-activate 'yank-pop)
  :custom
  (browse-kill-ring-show-preview nil)
  (counsel-yank-pop-preselect-last t)
  :config
  (bind-key "M-y" 'browse-kill-ring-forward browse-kill-ring-mode-map))

(use-package savehist
  :config
  (setq savehist-file
        (concat user-emacs-directory "savehist")
        savehist-additional-variables
        '(kill-ring))
  (savehist-mode t))

(use-package calendar
  :defer t
  :config
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  :custom
  (calendar-date-style (quote european)))

(defun update-dbus-session-bus-address ()
  (interactive)
  (setenv "DBUS_SESSION_BUS_ADDRESS"
          (with-temp-buffer
            (insert-file-contents "~/.dbus_session_bus_address")
            (buffer-string))))

(inotify-add-watch
 (expand-file-name "~/.dbus_session_bus_address")
 'close-write
 (lambda (_) (update-dbus-session-bus-address)))

(update-dbus-session-bus-address)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(bind-key "M-S-Q" 'unfill-paragraph)

(diminish 'defining-kbd-macro (propertize " M" 'face '(error bold)))

(use-package so-long
  :config
  (so-long-enable))

(use-package mode-line-bell
  :ensure t
  :config
  (mode-line-bell-mode 1))

(use-package buffer-table
  :bind ("C-x C-b" . buffer-table))

;; in lucid at least, emacs has a wobbly if it wants to display a
;; window atop a fullscreen window.
(defun i3-disable-fullscreen (&rest args)
  ;; if any frame is fullscreen, unfullscreen it
  (dolist (f (frame-list))
    (let ((fullscreen (frame-parameter f 'fullscreen)))
      (when (memq fullscreen '(fullscreen fullboth))
        (let ((fullscreen-restore (frame-parameter nil 'fullscreen-restore)))
	  (if (memq fullscreen-restore '(maximized fullheight fullwidth))
	      (set-frame-parameter f 'fullscreen fullscreen-restore)
	    (set-frame-parameter f 'fullscreen nil)))))))

(advice-add 'x-popup-menu   :before 'i3-disable-fullscreen)
(advice-add 'x-popup-dialog :before 'i3-disable-fullscreen)
(advice-add 'x-create-frame :before 'i3-disable-fullscreen)
