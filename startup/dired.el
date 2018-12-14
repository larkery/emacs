(defvar diredfl-ignore-compressed-flag nil)

(use-package diredfl
  :ensure t
  :commands diredfl-mode
  :config
  (setq diredfl-ignore-compressed-flag nil
        diredfl-compressed-extensions nil))

(use-package wdired
  :defer t
  :bind (:map dired-mode-map
              ("<f2>" . wdired-change-to-wdired-mode))
  :config
  (setq wdired-allow-to-change-permissions t))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
              ("M-n" . dired-next-subdir)
              ("M-p" . dired-prev-subdir)
              ("^" . dired-up-directory-here)
              ("I" . dired-replace-subdir)
              ("<tab>" . dired-maybe-replace-subdir)
              ("K" . dired-remove-subdir)
              ("e" . dired-xdg-open)
              ("C-x C-f" . dired-C-x-C-f)
              )
  :config
  (setq  dired-auto-revert-buffer t
         dired-bind-info nil
         dired-bind-jump nil
         dired-bind-man nil
         dired-dwim-target t
         dired-isearch-filenames 'dwim
         dired-listing-switches "-lahgG"
         dired-omit-files "^\\.[^\\.]"
         dired-omit-verbose nil
         dired-subtree-line-prefix
         (lambda (d)
           (concat (make-string (* 3 d) ? ) (propertize ">"
                                                        'face (intern
                                                               (concat "outline-"
                                                                       (number-to-string (mod d 9))
                                                                       )
                                                               )
                                                        ))
           )
         dired-subtree-use-backgrounds nil
         dired-subtree-line-prefix-face 'subtree
         )

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

    (dolist (file (dired-get-marked-files t current-prefix-arg))
      (start-process "xdg-open" nil "xdg-open" file)))
  
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
  
  )

(use-package dired-x
  :defer t
  :commands dired-omit-mode
  :bind (:map dired-mode-map
              (")" . dired-omit-mode)))

(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
              ("r" . dired-rsync)))

(defun dired-here-please ()
  (interactive)
  (if (eq major-mode 'dired-mode) (quit-window)
    (let* ((projectile-require-project-root nil)
           (file (or (buffer-file-name) default-directory))
           (pr (projectile-project-root))
           (here (if (file-directory-p file) file (file-name-directory file)))
           (there (or pr (if (file-directory-p here)
                             here
                           (file-name-directory here))))
           (dired-buffer (save-window-excursion (dired there))))
      (pop-to-buffer dired-buffer)
      (dolist (dir
               (cl-loop until (string= here there)
                        collect here
                        do (setq here (file-name-directory (directory-file-name here)))))
        (dired-maybe-insert-subdir dir))
      (dired-goto-file file))))

(bind-key "<f7>" 'dired-here-please)
