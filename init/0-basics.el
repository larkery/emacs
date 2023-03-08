(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(transient-mark-mode 1)

(setq tooltip-reuse-hidden-frame t
      tooltip-delay 1)


(setq ring-bell-function
      (lambda ()
        (call-process
         "paplay"
         nil 0 nil
         "/run/current-system/sw/share/sounds/freedesktop/stereo/dialog-warning.oga")))

(setq window-divider-default-places t 
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      default-frame-scroll-bars nil
      default-frame-alist '((scroll-bar-width . 1)
                            (right-divider-width . 3))

      visible-bell nil
      
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
      initial-scratch-message
      ";; May all sentient beings have happiness and the causes of happiness;
;; May all sentient beings be free from suffering and the causes of suffering;
;; May all sentient beings never be separated from the happiness that knows no suffering;
;; May all sentient beings live in equanimity, free from attachment and aversion.
;; ☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸☸

"

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
 '(fringe-mode '(6 . 8)))

(setq-default scroll-bar-width 8
	      indent-tabs-mode nil
	      case-fold-search t
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bda t
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

(defun other-window-or-split ()
  (interactive)
  (if (= 1 (count-windows))
      (progn
        (select-window (split-window-sensibly))
        (switch-to-next-buffer))
    (call-interactively 'other-window)))

(global-set-key (kbd "M-o") 'other-window-or-split)

(global-set-key (kbd "M-*") 'query-replace-regexp)
(global-set-key (kbd "C-*") 'query-replace)

(defun cycle-space ()
  (interactive) (cycle-spacing -1 t))

(global-set-key [remap just-one-space] 'cycle-space)

(setq auto-revert-interval 1)
(global-auto-revert-mode 1)

(setq revert-without-query (list (rx (* any))))

(use-package diminish :ensure t)

(use-package ivy
  :diminish
  :ensure t
  :defer nil
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
                                  (project-current-name))))))
             (dname (or (and b
                             (not (equal typ "%"))
                             (with-current-buffer b
                               (and file
                                    (if pname
                                        (file-relative-name file (project-current-root))
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

  (run-with-idle-timer 30 t 'recentf-save-list)

  :custom
  (recentf-exclude
   (quote
    ("^/home/hinton/notes/agenda" "^/nix/store")))
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

  (defun counsel-yank-pop-unles-yanking (o &rest args)
    (if (eq last-command 'yank)
        (apply o args)
      (counsel-yank-pop)))

  (advice-add 'yank-pop :around 'counsel-yank-pop-unles-yanking)

  :custom
  (counsel-find-file-ignore-regexp "\\`\\."))

(use-package lacarte
  :bind (("<f10>" . lacarte-execute-menu-command)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style (quote forward) nil (uniquify)))

(use-package paren
  :config
  (show-paren-mode 1))

;; (use-package yascroll
;;   :ensure t
;;   :config
;;   (global-yascroll-bar-mode 1))

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

(defvar dark-theme 'tango-dark)
(defvar light-theme 'tango)

(use-package modus-themes
  :ensure t
  :init (setq modus-themes-mode-line '(3d)
              ;modus-themes-completions 'moderate
              modus-themes-bold-constructs t
              modus-themes-syntax '(faint)
              modus-themes-fringes 'subtle
              light-theme 'modus-operandi
              dark-theme 'modus-vivendi)

)


(let ((theme (condition-case nil
                 (with-temp-buffer
                   (insert-file-contents "~/.config/cur-theme")
                   (buffer-string))
               (error "light"))))
  (load-theme (if (string= theme "light")
                  light-theme
                dark-theme)
              t))

(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

(defun switch-theme ()
  (interactive)
  (let* ((light-mode (not (custom-theme-enabled-p dark-theme)))
         (target-theme (if light-mode dark-theme light-theme))
         (inhibit-redisplay t))
    (change-theme target-theme t)
    (with-temp-buffer
      (insert (if light-mode "dark" "light"))
      (write-region (point-min) (point-max) "~/.config/cur-theme"))))

(bind-key "<f6>" 'switch-theme)

(setq-default line-spacing nil)

(use-package visual-line
  :commands visual-line-mode
  :init
  (add-hook 'text-mode-hook 'visual-line-mode)
  :custom
  (visual-line-fringe-indicators '(nil right-curly-arrow)))

(use-package savehist
  :config
  (setq savehist-file
        (concat state-directory "savehist")
        savehist-additional-variables
        '(kill-ring))
  (savehist-mode t))

(use-package calendar
  :defer t
  :config
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  :custom
  (calendar-date-style (quote european)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(bind-key "M-Q" 'unfill-paragraph)

(diminish 'defining-kbd-macro (propertize " M" 'face '(error bold)))

;; (use-package so-long
;;   :config
;;   (so-long-enable))

;; (use-package mode-line-bell
;;   :ensure t
;;   :config
;;   (mode-line-bell-mode 1))


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

(defun narrow-to-thing ()
  (interactive)

  (cond
   ((region-active-p)
    (narrow-to-region (mark) (point)))

   ((and smartparens-mode
         (let ((here (point))
               (back-thing (sp-get-thing)))
           (= here (plist-get back-thing :beg))))
    (save-mark-and-excursion
      (sp-mark-sexp)
      (beginning-of-line)
      (narrow-to-region (mark) (point))
      (deactivate-mark)))
      
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
      (narrow-to-region (mark) (point))))

   t
   (message "Not sure what to narrow to")
   ))

(bind-key "C-x n n" #'narrow-to-thing)

;; quiet auto-revert down
(advice-add
 'auto-revert-handler
 :around (lambda (orig-fun &rest args)
           (let ((auto-revert-verbose (not (minibufferp (window-buffer)))))
             (apply orig-fun args))))

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(when (fboundp 'malloc-trim)
  (defun gc-and-trim ()
    (interactive)
    (garbage-collect)
    (malloc-trim))

  (run-with-idle-timer 5 t 'gc-and-trim))


(defun diary-today ()
  (interactive)
  (let ((path (format-time-string
               "~/notes/Diary/%Y/%m/%d.md")))
    (find-file path)
    (goto-char (point-max))))

(bind-key "C-c d" #'diary-today)
