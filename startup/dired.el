(defvar diredfl-ignore-compressed-flag nil)

(use-package diredfl
  :ensure t
  :commands diredfl-mode
  :custom
  (diredfl-ignore-compressed-flag nil)
  (diredfl-compressed-extensions nil))

(use-package wdired
  :defer t
  :commands wdired-change-to-wdired-mode
  :custom
  (wdired-allow-to-change-permissions t))

(use-package dired
  :defer t
  :bind (("C-x d" . dired-from-buffer)
         :map dired-mode-map
              ("M-n" . dired-next-subdir)
              ("M-p" . dired-prev-subdir)
              ("^" . dired-up-directory-here)
              ("I" . dired-replace-subdir)
              ("<tab>" . dired-maybe-replace-subdir)
              ("K" . dired-remove-subdir)
              ("e" . dired-xdg-open)
              ("/" . find-dired)
              ("C-x C-f" . dired-C-x-C-f)
              ("f" . nil)
              ("f n" . find-name-dired-here)
              ("f g" . find-grep-dired-here))
  
  :commands dired-from-buffer
  :custom
  (dired-auto-revert-buffer t)
  (dired-bind-info nil)
  (dired-bind-jump nil)
  (dired-bind-man nil)
  (dired-dwim-target t)
  (dired-isearch-filenames 'dwim)
  (dired-listing-switches "-lahgG")
  (dired-omit-files "^\\.[^\\.]")
  (dired-omit-verbose nil)
  (dired-subtree-line-prefix 'dired-subtree-indented-arrow-prefix)
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix-face 'subtree)
  (directory-free-space-args "-PkH")
  :config

  (defun find-name-dired-here (pattern)
    (interactive "sPattern: ")
    (find-name-dired default-directory pattern))

  (defun find-grep-dired-here (pattern)
    (interactive "sRegexp: ")
    (find-grep-dired default-directory pattern))
  
  (defun with-ignored-errors (o &rest args)
    (ignore-errors
      (apply o args)))

  (advice-add 'get-free-disk-space :around 'with-ignored-errors)
  
  (major-mode-hydra-define dired-mode
    (:quit-key "q")
    ("Tools"
     (("<f2>" wdired-change-to-wdired-mode "edit names"))

     "File"
     (("S" dired-do-symlink "symlink (absolute)")
      ("Y" dired-do-relsymlink "symlink (relative)"))

     "Image"
     (("i t" image-dired-toggle-marked-thumbs "thumbs")
      ("i d" image-dired-display-thumbs "display"))

     
     ))

  (defun dired-from-buffer ()
    (interactive)
    (let ((b buffer-file-name)
          (d (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))
      (when d
        (with-current-buffer (dired d)
          (when b (dired-goto-file b))))))
  
  (defun dired-subtree-indented-arrow-prefix (d)
      (concat (make-string (* 3 d) ? )
              (propertize ">" 'face (intern (concat "outline-" (number-to-string (mod d 9)))))))
    
  (require 'dired-parent-links)
  
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (add-hook 'dired-mode-hook 'diredfl-mode)

  (add-hook 'dired-mode-hook 'dired-omit-mode)
  
  (defun dired-C-x-C-f ()
    (interactive)

    (let ((default-directory (dired-current-directory)))
      (call-interactively (global-key-binding (kbd "C-x C-f")))))
  
  (defun dired-xdg-open ()
    "Open the file at point with xdg-open"
    (interactive)
    (let* ((files (dired-get-marked-files t current-prefix-arg))
           (nfiles (length files)))
      (when (or (< nfiles 8)
                (y-or-n-p (format "Really open %d files?" nfiles)))
        
        (dolist (file files)
          (start-process "xdg-open" nil "xdg-open" file)))))
  
  (defun dired-remove-subdir (arg)
    "Remove the subdir at point, or with C-u remove all of them"
    (interactive "P")
    (if arg
        (save-excursion
          (goto-char (point-min))
          (while (dired-next-subdir (point))
            (dired-kill-subdir)
            (goto-char (point-min))))
      (let ((here (dired-current-directory)))
        (and (dired-goto-subdir here)
             (progn (dired-do-kill-lines 1)
                    (dired-goto-file here))))))

  (defun dired-maybe-replace-subdir ()
    "Insert the subdirectory, replacing this directory unless this is the root."
    (interactive)
    (if (cdr dired-subdir-alist)
        (let ((here (dired-current-directory)))

          (call-interactively #'dired-maybe-insert-subdir)
          (save-excursion
            (and (dired-goto-subdir here)
                 (dired-do-kill-lines 1))))
      (call-interactively #'dired-maybe-insert-subdir)))
  
  (defun dired-replace-subdir ()
    "Insert the subdirectory, replacing this directory if possible"
    (interactive)
    (if (cdr dired-subdir-alist)
        (let ((here (dired-current-directory)))

          (call-interactively #'dired-maybe-insert-subdir)
          (save-excursion
            (and (dired-goto-subdir here)
                 (dired-do-kill-lines 1))))
      (dired-find-alternate-file)))

  (defun dired-up-directory-here (arg)
    "Go up a directory, replacing the current directory if possible"
    (interactive "P")
    (let* ((here (dired-current-directory))
           (up (file-name-directory (directory-file-name here))))
      (or (dired-goto-file (directory-file-name here))
          (and (cdr dired-subdir-alist)
               (or (dired-goto-subdir up)
                   (condition-case nil
                       (progn
                         (dired-goto-subdir here)
                         (dired-do-kill-lines 1)
                         (dired-maybe-insert-subdir up) t)
                     (error nil))))
          (progn (find-alternate-file up)
                 (dired-goto-file here)))))

  (defun dired-kill-before-delete (file &rest rest)
    (let ((buf (get-file-buffer file)))
      (when buf
        (kill-buffer buf)
        (dolist (dired-buf (dired-buffers-for-dir file))
          (kill-buffer dired-buf)))))

  (advice-add 'dired-delete-file :before 'dired-kill-before-delete)

  (defun dired-click (event)
    (interactive "e")

    (let (window pos file)
      (save-excursion
        (setq window (posn-window (event-end event))
	      pos (posn-point (event-end event)))
        (if (not (windowp window))
	    (error "No file chosen"))
        (set-buffer (window-buffer window))
        (goto-char pos)
        (setq file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (progn
            (select-window window)
            ;; TODO handle .. and . in some way, and the header lines
            (call-interactively 'dired-maybe-replace-subdir))

        (select-window window)
        (find-file-other-window (file-name-sans-versions file t))))
    )


  (bind-key [mouse-2] 'dired-click dired-mode-map)          
        
  (defun recentf-add-dired-directory ()
    (when (and (stringp dired-directory)
               (equal "" (file-name-nondirectory dired-directory)))
      (recentf-add-file dired-directory)))
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)

  )

(use-package wdired
  :bind
  (:map dired-mode-map
        (";" . dired-toggle-read-only)))


(use-package dired-x
  :defer t
  :commands dired-omit-mode
  :bind (:map dired-mode-map
              (")" . dired-omit-mode)))

(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
              ("r" . dired-rsync)))

(bind-key "<f7>" (lambda () (interactive) (dired default-directory)))

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :diminish
  :hook
  (dired-mode . all-the-icons-dired-mode))

