(define-prefix-command 'leader-keys)

(defvar leader-key "<escape>")
(defvar leader-key* "§")

(bind-keys
 :map leader-keys
 ("w w" . delete-other-windows)
 ("w f" . window-toggle-full-frame)
 ("w q" . delete-window)
 ("w s" . split-window-below)
 ("w e" . split-window-right)
 ("b k" . kill-current-buffer)
 ("b b" . ivy-switch-buffer)
 ("t s" . ispell-word)
 ("t r" . anzu-query-replace-regexp)
 ("f s" . save-buffer)
 ("f r" . counsel-recentf)
 ("f f" . find-file)
 ("e e" . eval-expression)
 ("s"   . split-window-80c)
 ("q"   . delete-window-or-bury)
 ("a"   . delete-other-windows))

(defun delete-window-or-bury ()
  (interactive)
  (if (one-window-p)
      (bury-buffer)
    (delete-window)))

(defun window-toggle-full-frame ()
  (interactive)
  (let* ((frm (selected-frame))
         (is-full-frame (frame-parameter frm 'full-frame-window)))
    (if is-full-frame
        (progn (set-window-configuration is-full-frame)
               (set-frame-parameter frm 'full-frame-window nil))
      (progn
        (set-frame-parameter frm 'full-frame-window (current-window-configuration ))
        (delete-other-windows)))))

(defun split-window-80c ()
  (interactive)
  (cl-labels ((horizontal-window-count
               (tree)
               (if (atom tree)
                   1
                 (if (car tree)
                     (apply 'max (mapcar #'horizontal-window-count (cddr tree)))
                   (apply '+ (mapcar #'horizontal-window-count (cddr tree)))))))
    (let* ((frame (selected-frame))
           (ncols (horizontal-window-count (car (window-tree frame))))
           (cols-per-vert (/ (frame-width frame) (1+ ncols))))
      (if (>= cols-per-vert 80)
          (split-window-right)
        (split-window-below)))))

(defun leader-kbd (s) (kbd (concat leader-key* " " s)))
(defun leader-name (s n)
  (which-key-add-key-based-replacements
    (concat leader-key* " " s) n))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
   (concat leader-key* " w") "windows"
   (concat leader-key* " t") "text"
   (concat leader-key* " b") "buffer"
   (concat leader-key* " f") "file"
   (concat leader-key* " e") "eval"))

(defun leader-passthrough ()
  (interactive)
  (call-interactively
   (or
    (local-key-binding  (kbd leader-key))
    (global-key-binding (kbd leader-key)))))

(bind-key leader-key #'leader-tab leader-keys)

(defun leader-keys* ()
  (interactive)
  (setq unread-command-events (listify-key-sequence
                               (kbd leader-key*))))

(define-minor-mode leader-keys-mode "Enable my leader keys"
  t " L"
  (let ((m (make-sparse-keymap)))
    ;; TODO work out how to make tab do what tab would do if this map
    ;; was off
    (define-key m (kbd leader-key*) 'leader-keys)
    (define-key m (kbd leader-key) #'leader-keys*)
    m))

(defun bind-leader (key cmd &optional map)
  (if map
      (bind-key (concat leader-key* " " key) cmd map)
    (bind-key key cmd leader-keys)))



