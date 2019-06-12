(define-prefix-command 'leader-keys)

(defvar leader-key (kbd "<tab>"))
(defvar leader-key* (kbd "§"))

(bind-keys
 :map leader-keys
 ("w w" . delete-other-windows)
 ("w q" . delete-window)
 ("w s" . split-window-below)
 ("w e" . split-window-right)
 ("k" . kill-current-buffer)
 ("t s" . ispell-word)
 ("t r" . anzu-query-replace-regexp)
 ("s" . save-buffer))

(which-key-add-key-based-replacements
  (concat leader-key* " w") "windows"
  (concat leader-key* " t") "text")

(defun leader-tab ()
  (interactive)
  (call-interactively (or
                       (local-key-binding (kbd "TAB"))
                       (global-key-binding (kbd "TAB")))))

(bind-key leader-key #'leader-tab leader-keys)

(defun leader-keys* ()
  (interactive)
  (setq unread-command-events (listify-key-sequence leader-key*)))


(define-minor-mode leader-keys-mode "Enable my leader keys"
  t " L"
  (let ((m (make-sparse-keymap)))
    ;; TODO work out how to make tab do what tab would do if this map
    ;; was off
    (define-key m leader-key* 'leader-keys)
    (define-key m leader-key #'leader-keys*)
    m))

