(defun menu-to-alist (keymap &optional events root menu-alist)
  (map-keymap
   (lambda (event binding)
     (let ((is-menu-item (eq 'menu-item (car-safe binding)))
           (item-name (car-safe (cdr-safe binding))))
      (cond
       ((or ;; these are all junk conditions
         (and is-menu-item
              (null (cdr-safe (cdr-safe binding))))
         (and (stringp (car-safe binding))
              (null (cdr-safe binding)))
         (and is-menu-item
              (stringp item-name)
              (string-match "\\`--" item-name)))
        
        (setq binding nil))

       ((and is-menu-item
             (member :filter (cdr (cddr binding))))
        (setq binding
              (let ((filter (cadr (member :filter (cdr (cddr binding))))))
                (if (functionp filter)
                    (funcall filter (car (cddr binding)))
                  (car (cddr binding))))))

       (is-menu-item
        (let ((enable-condition (memq ':enable (cdr-safe (cdr-safe (cdr-safe binding))))))
          (if (or (not enable-condition)
                  (condition-case nil (eval (cadr enable-condition)) (error nil)))
              (setq item-name (eval (cadr binding))
                    binding (car-safe (cdr-safe (cdr-safe binding))))
            (setq binding nil))))

       ((stringp (car-safe binding))
        (setq item-name (eval (car binding)))
        (setq binding   (cdr binding))
        ;; Skip HELP-STRING
        (when (stringp (car-safe binding)) (setq binding  (cdr binding)))
        ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
        (when (and (consp binding)  (consp (car binding)))
          (setq binding  (cdr binding)))))

      ;; at this point item-name and binding should be appropriate
      (cond
       ((keymapp binding)
        ;; do a recursion
        (setq menu-alist
              (menu-to-alist binding
                             (cons event events)
                             (concat root (and root " > ") item-name)
                             menu-alist)))
       ((commandp binding)
        ;; add it in
        (push
         (list (concat root (and root " > ") item-name)
               binding
               (cons event events)
               )
         menu-alist)))
      menu-alist))
   keymap)
  menu-alist)

(defun completing-read-popup-menu (position menu)
  (when position
    (cond
     ((keymapp menu)
      
      (let* ((choices (nreverse (menu-to-alist menu)))
             (choice
              (completing-read
               (concat (keymap-prompt menu) ": ")
               choices))
             )
        (reverse (caddr (assoc choice choices)))))
     
     ((and (listp menu) (keymapp (car menu)))
      (let* ((choices (nreverse (loop for keymap in menu
                       concat (menu-to-alist keymap))))
             (choice
              (completing-read
               "Menu: "
               choices)))
        (reverse (caddr (assoc choice choices))))))))

(advice-add 'x-popup-menu :override 'completing-read-popup-menu)

;;(advice-remove 'x-popup-menu 'completing-read-popup-menu)
