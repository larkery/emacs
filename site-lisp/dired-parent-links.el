(require 'dired)

(defface dired-mouseover-face
  '((t (:inherit link)))
  "Face for `dired-mouseover-face'."
  :group 'dired)

(defvar dired-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'dired-follow-link)
    (define-key map [return] 'dired-follow-link)
    (define-key map [follow-link] 'mouse-face)
      map)
  "Keymap for mouse when in `dired-mode'.")

;; Author:  Drew Adams -- http://emacs.stackexchange.com/a/13411/2287
(defun dired-follow-link (event)
"Follow the link in the dired directory heading, causing a new
dired buffer to be opened."
  (interactive (list last-nonmenu-event))
  (run-hooks 'mouse-leave-buffer-hook)
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (let ((path  (get-text-property (posn-point (event-start event)) 'breadcrumb)))
      (dired path))))

(defun dired-propertize-directory-heading ()
(interactive)
  (unless (buffer-narrowed-p)
    (let* (
        p beg end path peol
        (inhibit-read-only t) )
      (save-excursion
        (goto-char (point-min))
        (setq peol (point-at-eol))
        (set-text-properties (point) peol nil)
        (re-search-forward "\\([^/\\]+\\)[/\\]" peol t)
        (when (looking-back "\\(^ +\\)\\([a-zA-Z]:\\)?/")
          (setq p (match-end 1))
          (setq path (if (match-string 2) (concat (match-string 2) "/") "/"))
          (add-text-properties (point-min) (1- (match-end 0)) (list
            'breadcrumb path
            'mouse-face 'dired-mouseover-face
            'help-echo (format "mouse-2, RET: Follow the link to \"%s\"." path)
            'keymap dired-mouse-map)))
        (while (re-search-forward "\\([^/\\]+\\)[/\\]" peol t)
          (setq beg (match-beginning 1))
          (setq end (match-end 1))
          (setq path (buffer-substring-no-properties p end))
          (add-text-properties beg end (list
            'breadcrumb path
            'mouse-face 'dired-mouseover-face
            'help-echo (format "mouse-2, RET: Follow the link to \"%s\"." path)
            'keymap dired-mouse-map)))
        (setq path (buffer-substring-no-properties p (1- peol)))
        (add-text-properties (point) (1- peol) (list
          'breadcrumb path
          'mouse-face 'dired-mouseover-face
          'help-echo (format "mouse-2, RET: Follow the link to \"%s\"." path)
          'keymap dired-mouse-map))))))

(add-hook 'dired-after-readin-hook 'dired-propertize-directory-heading)
(provide 'dired-parent-links)
