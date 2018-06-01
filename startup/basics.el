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
(global-set-key (kbd "<f5>") 'save-buffer)
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
  :bind ("M-s M-s" . swiper-at-point)
  :config (ivy-mode)
  (setq ivy-use-virtual-buffers t)

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
  
  (setq recentf-max-saved-items 100
        recentf-exclude '("^/home/hinton/notes/" "^/nix/store"
                          recentf-exclude-deleted-local-files))
  (custom-set-variables
   '(recentf-auto-cleanup 300)))

(use-package counsel
  :diminish
  :ensure t
  :bind (("C-x C-r" . counsel-recentf)
         :map counsel-mode-map
         ([remap yank-pop] . nil)
         )
  :config
  (counsel-mode 1)
  (setq counsel-find-file-ignore-regexp "\\`\\.")
  
  )

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

(use-package theme-to-xresources
  :config
  (theme-to-xresources)
  (defadvice load-theme (after update-xresources-after-load-theme activate)
    (theme-to-xresources)
    (set-fringe-mode '(0 . 8))))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-gruvbox-dark-hard t)
  (theme-to-xresources))


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
  (bind-key "M-y" 'browse-kill-ring-forward browse-kill-ring-mode-map))

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

