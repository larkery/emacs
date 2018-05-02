(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(set-fringe-mode 4)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)
(window-divider-mode 1)

(setq default-frame-scroll-bars nil
      default-frame-alist '((scroll-bar-width . 8)
                            (right-divider-width . 4))
      focus-follows-mouse t
      mouse-autoselect-window t
      echo-keystrokes 0.2
      disabled-command-function nil
      set-mark-command-repeat-pop t
      )

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
    (swiper (or (thing-at-point 'symbol)
                (thing-at-point 'word))))

  (defvar ivy--switch-buffer-transformer-format "%-50s%10s")

  (defun ivy--switch-buffer-mb-width (orig)
    (let* ((mb-width (window-width (minibuffer-window)))
           (ivy--switch-buffer-transformer-format
            (format "%%-%ds%%%ds"
                    (- mb-width 22)
                    20)))
      (funcall orig)))

  (advice-add 'ivy-switch-buffer :around 'ivy--switch-buffer-mb-width)
  
  (defun ivy-switch-buffer-transformer (str)
    (let ((b (get-buffer str)))
      (if b
          (with-current-buffer b
            (let ((file (or buffer-file-name dired-directory)))
              (if file
                  (let* ((project (if (projectile-project-p)
                                      (concat " " (projectile-project-name)) ""))
                         (file (or buffer-file-name dired-directory))
                         (host (if (and file (file-remote-p file))
                                   (concat "@" (tramp-file-name-host (tramp-dissect-file-name file)))
                                 ""))
                         (dir (if (and file (file-directory-p file)) "D" ""))
                         (misc (format "%-2s%-10s%10s" dir host project)))

                    (format ivy--switch-buffer-transformer-format str misc))
                (ivy-append-face str 'italic)
                )))
        str)
      ))
  )

(use-package counsel
  :diminish
  :ensure t
  :config (counsel-mode)
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

(use-package theme-to-xresources
  :config
  (theme-to-xresources)
  (defadvice load-theme (after update-xresources-after-load-theme activate)
    (theme-to-xresources)))

(use-package dakrone-light-theme
  :ensure t
  :config
  (load-theme 'dakrone-light t))

(use-package visual-line
  :commands visual-line-mode
  :init
  (add-hook 'text-mode-hook 'visual-line-mode)
  :config
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)))

