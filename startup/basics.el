(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)

(setq window-divider-default-places t 
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      default-frame-scroll-bars nil
      default-frame-alist '((scroll-bar-width . 1)
                            (right-divider-width . 3))

      frame-resize-pixelwise t
      window-resize-pixelwise t
      
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
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message ";; Only death is certain, and the time of death is unknown\n\n"

      enable-recursive-minibuffers t

      scroll-step 10

      auto-hscroll-mode 'current-line
      mouse-drag-and-drop-region t

      locate-dominating-stop-dir-regexp
      (purecopy (rx bos "/net/" (* (not (any "/"))) "/" (* (not (any "/"))) eos))

      view-read-only t
      )

(window-divider-mode 0)

(custom-set-variables
 '(calendar-date-style 'european)
 '(fringe-mode '(0 . 8)))

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

(use-package diminish :ensure t)

(use-package ivy
  :diminish
  :ensure t
  :defer nil
  :custom
  (ivy-use-virtual-buffers t)
  :config (ivy-mode)

  (with-eval-after-load
      'swiper 
    (bind-key "C-'" nil swiper-map))
  
  (defun shorten-text (text length keep-left)
    (if (> (length text) length)
        (concat
         (substring text 0 keep-left)
         "…"
         (substring text (- (length text)  (- length 1 keep-left))))
        text))
  
  (defun shorten-file-name (f l)
    (let ((f (abbreviate-file-name f)))
      (shorten-text f l 2)
      ))

  (defun ivy-switch-buffer-transformer (str)
    (if (< (window-width (minibuffer-window)) 75) str
      (let* ((b (get-buffer str))
             (file (and b
                        (with-current-buffer b
                          (or buffer-file-name dired-directory default-directory))))
             (remote (and file (file-remote-p file)))
             (typ (if b
                      (with-current-buffer b
                        (cond (dired-directory "/")
                              (remote "@")
                              (buffer-file-name "f")
                              (t "%")))
                    "v"))
             (pname (and b
                         (with-current-buffer b
                           (let ()
                             (and file
                                  (not remote)
                                  (projectile-project-name))))))
             (dname (or (and b
                             (not (equal typ "%"))
                             (with-current-buffer b
                               (and file
                                    (if pname
                                        (file-relative-name file (projectile-project-root))
                                      (abbreviate-file-name (file-name-directory file)))))
                             ) ""))
             )

        (format
         "%s %10s  %-30s   %s"
         (propertize typ 'face 'error)
         (propertize (shorten-text (or pname "-") 10 0) 'face 'shadow)
         str
         (propertize dname 'face 'shadow)
         )))))

(use-package recentf
  :config
  (recentf-mode 1)

  (defun recentf-excluded-deleted-local-files (fi)
    (unless (or (file-remote-p fi)
                (string-match-p (rx bos "/net") fi))
      (not (file-exists-p fi))))
  (run-with-idle-timer 600 t 'recentf-save-list)

  :custom
  (recentf-exclude
   (quote
   ("^/home/hinton/notes/agenda" "^/nix/store" recentf-exclude-deleted-local-files)))
  (recentf-max-saved-items 100)
  (recentf-auto-cleanup 300))

(use-package counsel
  :diminish
  :defer nil
  :ensure t
  :bind (("<f10>" . counsel-tmm)
         ("C-x C-r" . counsel-recentf)
         :map counsel-mode-map
         ([remap yank-pop] . nil)
         )
  :config
  (counsel-mode 1)

  (defun counsel-yank-pop-unles-yanking (o &rest args)
    (if (eq last-command 'yank)
        (apply o args)
      (counsel-yank-pop)))

  (advice-add 'yank-pop :around 'counsel-yank-pop-unles-yanking)

  (defun completing-read-prefix-bindings (&rest _)
    (let ((stuff (substring (concat (this-command-keys-vector)) 0 -1)))
      (counsel-descbinds stuff)))

  (advice-add 'describe-prefix-bindings :around #'completing-read-prefix-bindings)

  (defun tmm-get-flat-keymap (menu)
    (let ((tmm-km-list nil)
          submap)
      ;; mangle tmm-km-list
      (map-keymap (lambda (k v) (tmm-get-keymap (cons k v))) menu)

      ;; explode submenus of same
      (cl-loop for entry in tmm-km-list
               do (setq submap (cdr (cdr entry)))
               if (keymapp submap)
               append (let ((tmm-flat-keymap-prefix
                             (concat tmm-flat-keymap-prefix
                                     (car entry)
                                     " → ")
                             ))
                        (tmm-get-flat-keymap submap))
               else collect
               (cons
                (concat tmm-flat-keymap-prefix (car entry))
                (cdr entry)))))

  (defun counsel-tmm-prompt (menu)
    "Select and call an item from the MENU keymap."
    (let (out
          choice
          chosen-string
          tmm-flat-keymap-prefix)
      (setq tmm-km-list (tmm-get-flat-keymap menu))
      (setq out (ivy-read "Menu bar: " (tmm--completion-table tmm-km-list)
                          :require-match t))
      (setq choice (cdr (assoc out tmm-km-list)))
      (setq chosen-string (car choice))
      (setq choice (cdr choice))
      (cond ((keymapp choice)
             (counsel-tmm-prompt choice))
            ((and choice chosen-string)
             (setq last-command-event chosen-string)
             (call-interactively choice)))))

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
  
  (condition-case nil
      (run-hooks 'custom-theme-load-hook)
    (error nil)))

(defun run-custom-theme-load-hook-once (frame)
  (when (window-system frame)
    (run-hooks 'custom-theme-load-hook)
    (remove-hook 'after-make-frame-functions 'run-custom-theme-load-hook-once)))

(add-hook 'after-make-frame-functions 'run-custom-theme-load-hook-once)

(use-package theme-to-xresources
  :defer nil
  :commands theme-to-xresources
  :init
  (add-hook
   'custom-theme-load-hook
   (lambda ()
     (theme-to-xresources)
     (condition-case
      nil
      (call-process "i3" nil nil nil "reload")
      (error nil)))))

(defvar dark-theme 'wombat)
(defvar light-theme 'adwaita)

(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "site-lisp/themes"))

(use-package gruvbox-theme :ensure t)
;; (setq dark-theme 'gruvbox-dark-hard
;;       light-theme 'gruvbox-light-hard)


(setq dark-theme 'gruvbox-dark-soft
      light-theme 'gruvbox-light-soft)

(load-theme light-theme t)
(load-theme 'tweaks t)

(defun switch-theme ()
  (interactive)
  (let* ((light-mode (custom-theme-enabled-p light-theme))
         (target-theme (if light-mode dark-theme light-theme))
         (inhibit-redisplay t))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))

    (load-theme target-theme t)
    (load-theme 'tweaks t)))


(bind-key "<f6>" 'switch-theme)

(setq-default line-spacing nil)

(use-package visual-line
  :commands visual-line-mode
  :init
  (add-hook 'text-mode-hook 'visual-line-mode)
  :custom
  (visual-line-fringe-indicators '(nil right-curly-arrow)))

;; (use-package browse-kill-ring
;;   :commands browse-kill-ring
;;   :ensure t
;;   :init
;;   (defadvice yank-pop (around kill-ring-browse-maybe (arg))
;;     "If last action was not a yank, run `browse-kill-ring' instead."
;;     ;; pinched from browse-kill-ring, so we don't have to autoload it at startup
;;     (interactive "p")
;;     (if (not (eq last-command 'yank))
;;         (counsel-yank-pop)
;;       (barf-if-buffer-read-only)
;;       ad-do-it))
;;   (ad-activate 'yank-pop)
;;   :custom
;;   (browse-kill-ring-show-preview nil)
;;   (counsel-yank-pop-preselect-last t)
;;   :config
;;   (bind-key "M-y" 'browse-kill-ring-forward browse-kill-ring-mode-map))


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
  
  (condition-case nil
      (setenv "DBUS_SESSION_BUS_ADDRESS"
              (with-temp-buffer
                (insert-file-contents "~/.dbus_session_bus_address")
                (buffer-string)))
    (error nil)))

(condition-case nil
    (inotify-add-watch
     (expand-file-name "~/.dbus_session_bus_address")
     'close-write
     (lambda (_) (update-dbus-session-bus-address)))
  (error nil))

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
  (so-long-enable)

  ;; (so-long-disable)
  )

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

(use-package sh-script
  :config
  ;; for reasons unknown SMIE breaks in shell indentation at the moment
  (setq sh-use-smie nil))

(use-package mixed-pitch
  :ensure t
  :config
  (add-hook 'text-mode-hook #'mixed-pitch-mode)
  (add-hook 'notmuch-show-mode-hook #'mixed-pitch-mode)
  (add-hook 'message-mode-hook #'mixed-pitch-mode)
  )


(defun narrow-to-thing ()
  (interactive)

  (cond
   ((eq 'org-mode major-mode)
    (cond
     ((org-at-block-p)
      (org-narrow-to-block))

     (t (org-narrow-to-subtree))))
   
   ((derived-mode-p 'prog-mode)
    (narrow-to-defun))

   ((derived-mode-p 'text-mode)
    (save-mark-and-excursion
      (mark-paragraph)
      (narrow-to-region)))
   
   (t (narrow-to-region))))

(bind-key "C-x n n" #'narrow-to-thing)

(advice-add
 'auto-revert-handler
 :around (lambda (orig-fun &rest args)
           (let ((auto-revert-verbose (not (minibufferp (window-buffer)))))
              (apply orig-fun args))))
