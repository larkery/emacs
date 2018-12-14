(use-package nix-mode :ensure t :defer t)

(use-package pdf-tools
  :defer t
  :commands pdf-view-mode pdf-view-word-document
  :config
  
  (defun pdf-view-word-document ()
    (let ((tempfile (make-temp-file "word-pdf" nil ".doc")))
      (write-region (point-min) (point-max) tempfile)
      (let ((default-directory
              (file-name-directory tempfile)
              ))
        (shell-command (format "soffice --headless --convert-to pdf %s" (shell-quote-argument tempfile))))
      (erase-buffer)
      (insert-file-contents-literally
       (replace-regexp-in-string "\\.doc$" ".pdf" tempfile)
       t)
      (pdf-view-mode))))

