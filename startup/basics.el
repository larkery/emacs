(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(set-fringe-mode '(0 . 8))
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)
(window-divider-mode 1)

(setq default-frame-scroll-bars nil
      default-frame-alist '((scroll-bar-width . 8)
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
      )

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
  
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers t)

  (with-eval-after-load
      'swiper 
    (bind-key "C-'" nil swiper-map))
  
  (defun swiper-at-point ()
    (interactive)
    (swiper (when (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end)))))

  (defvar ivy--switch-buffer-format "%s %-50s%10s")

  (defun ivy--switch-buffer-mb-width (orig)
    (let* ((mb-width (window-width (minibuffer-window)))
           (ivy--switch-buffer-format
            (format "%%s %%-%ds%%%ds" (- mb-width 40) 38)))
      (funcall orig)))

  (advice-add 'ivy-switch-buffer :around 'ivy--switch-buffer-mb-width)
  
  (defun ivy-switch-buffer-transformer (str)
    (let ((b (get-buffer str)))
      (if b
          (with-current-buffer b
            (let* ((proj (if (projectile-project-p)
                             (projectile-project-name)))
                   (file (or buffer-file-name dired-directory))
                   (host (if (and file (file-remote-p file))
                             (concat "@"
                                     (tramp-file-name-host
                                      (tramp-dissect-file-name
                                       (or buffer-file-name dired-directory))))
                           "")))
              (cond
               (dired-directory
                (format ivy--switch-buffer-format ":" str (or proj host)))
               
               (buffer-file-name
                (format ivy--switch-buffer-format "." str (or proj host)))

               (t (format ivy--switch-buffer-format "%" str (or proj host)))
               
               )))
        (format ivy--switch-buffer-format "v" str "")
        )))
    )

(use-package recentf
  :config
  (recentf-mode 1)

  (defun recentf-excluded-deleted-local-files (fi)
    (unless (file-remote-p fi)
      (not (file-exists-p fi))))
  (run-with-idle-timer 600 t 'recentf-save-list)
  (setq recentf-max-saved-items 100
        recentf-exclude '("^/home/hinton/notes/" "^/nix/store"
                          recentf-exclude-deleted-local-files))
  (custom-set-variables
   '(recentf-auto-cleanup 300)))

(use-package counsel
  :diminish
  :defer nil
  :ensure t
  :bind (("C-x C-r" . counsel-recentf)
         :map counsel-mode-map
         ([remap yank-pop] . nil))
  :config
  (counsel-mode 1)
  (setq counsel-find-file-ignore-regexp "\\`\\."))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

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

(use-package gruvbox-theme :ensure t)
(use-package solarized-theme :ensure t)

(setq dark-theme 'gruvbox)
(setq light-theme 'solarized-light)

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
  :config
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)))

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
  :config
  (setq browse-kill-ring-show-preview nil)
  (setq counsel-yank-pop-preselect-last t)
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
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

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

(setq visible-bell t)

(diminish 'defining-kbd-macro (propertize " M" 'face '(error bold)))

(use-package so-long
  :config
  (so-long-enable))

