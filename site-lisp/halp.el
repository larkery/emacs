;; -*- lexical-binding: t -*-

(require 'quick-peek)

(cl-defgeneric halp-text (provider)
  "Get short help text for the thing at point"
  nil)

(cl-defgeneric halp-open (provider)
  "Open a help buffer for the thing at point"
  (error "No help method known for %s" provider))

(defvar halp-backend nil)
(make-variable-buffer-local 'halp-backend)


(defun halp-inline ()
  (interactive)
  (let* ((backend (or halp-backend major-mode))
         (text (halp-text backend))
         (key (edmacro-format-keys (vector last-input-event)))
         stop)
    (if (not text)
        (halp-open backend)
      (quick-peek-show text)
      (message "SPC to expand, %s to open a window" key))
    (setq stop
          (set-transient-map
           (let ((m (make-sparse-keymap)))
             (define-key m
               (kbd key) (lambda () (interactive)
                           (quick-peek-hide)
                           (funcall stop)
                           (halp-open backend)
                           ))
             (define-key m
               (kbd "SPC")  (lambda () (interactive)
                              (quick-peek-hide)
                              (recenter 1)
                              (quick-peek-show text nil nil 'none)))
             m)
           t
           #'quick-peek-hide))))

(cl-defmethod halp-text ((provider (eql emacs-lisp-mode)))
  (when-let ((sym (thing-at-point 'symbol)))
    (when-let ((sym (intern sym)))
      (or
       (condition-case nil (documentation sym) (error nil))
       (condition-case nil
           (documentation-property sym 'variable-documentation)
           (error nil))))))

(cl-defmethod halp-open ((provider (eql emacs-lisp-mode)))
  (let ((help-window-select t))
    (describe-symbol (intern (thing-at-point 'symbol)))))

(cl-defmethod halp-text ((provider (eql sh-mode)))
  (shell-command-to-string (format "man --whatis %s" (thing-at-point 'symbol))))

(cl-defmethod halp-open ((provider (eql sh-mode)))
  (let ((help-window-select t))
    (pop-to-buffer (man (thing-at-point 'symbol)))))

(cl-defmethod halp-text ((provider (eql shell-mode)))
  (shell-command-to-string (format "man --whatis %s" (thing-at-point 'symbol))))

(cl-defmethod halp-open ((provider (eql shell-mode)))
  (let ((help-window-select t))
    (pop-to-buffer (man (thing-at-point 'symbol)))))

(cl-defmethod halp-open ((provider (eql clojure-mode)))
  (browse-url (format "https://clojuredocs.org/clojure.core/%s"
                      (thing-at-point 'symbol))))

(cl-defmethod halp-text ((provider (eql dictionary)))
  (dictionary-definition (thing-at-point 'word)))

(cl-defmethod halp-open ((provider (eql dictionary)))
  (dictionary-search (thing-at-point 'word)))

(add-hook 'text-mode-hook
          (lambda () (setq halp-backend 'dictionary)))

(add-hook 'org-mode-hook
          (lambda () (setq halp-backend 'dictionary)))

(provide 'halp)
