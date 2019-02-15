(require 'bs)
(require 'projectile)

(defvar bs-projectile-project nil)

(bind-key "C-x C-b" 'bs-projectile projectile-mode-map)

(defun bs-project-name ()
  (let ((ppr (projectile-project-root)))
    (and ppr (abbreviate-file-name ppr))))

(defun in-bs-projectile-project (buf)
  (with-current-buffer buf
    (string= bs-projectile-project (bs-project-name))
    ))

(defun not-in-bs-projectile-project (buf)
  (not (in-bs-projectile-project buf))

  )

(setq bs-configurations
      (cons
       '("project"
         nil
         nil ;;in-bs-projectile-project
         nil
         not-in-bs-projectile-project
         nil)
       bs-configurations))

(defun bs-projectile ()
  (interactive)
  (setq bs-projectile-project (bs-project-name))
  (let ((bs-default-configuration "project"))
    (bs-show nil))
  
  )

(defun bs-projectile-next ()
  (interactive)
  (let* ((ps (projectile-open-projects))
         (p1 (car ps))
         (pn (cadr (member bs-projectile-project ps))))
    (if (not bs-projectile-project)
        (setq bs-projectile-project p1)
      (setq bs-projectile-project pn)
      )
    (message "-> %s" bs-projectile-project)
    (bs-refresh)))

(bind-key "TAB" 'bs-projectile-next bs-mode-map)

(provide 'bs-projectile)
