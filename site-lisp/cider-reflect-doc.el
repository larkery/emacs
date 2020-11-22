
(defvar cider-reflect-form
"(do
  (try
    (require 'clojure.reflect)
    (require 'clojure.string)
    (let [m (:members (clojure.reflect/reflect %s))]
      (group-by :kind (for [m m 
                            :when ((:flags m) :public)
                            :let [static ((:flags m) :static)]]
        (cond 
          (instance? clojure.reflect.Method m)
          {:kind (if static :static-method :method)
           :name (str (:name m))
           :returns (str (:return-type m))
           :parameters (map str (:parameter-types m))
           :throws (map str (:exception-types m))
           :declaring-class (str (:declaring-class m))
           }
          
          (instance? clojure.reflect.Field m)
          {:kind (if static :static-field :field)
           :name (str (:name m))
           :type (str (:type m))
           :declaring-class (str (:declaring-class m))
           }
          
          (instance? clojure.reflect.Constructor m)
          {:kind :constructor
           :parameters (map str (:parameter-types m))
           :throws (map str (:exception-types m))
           :declaring-class (str (:declaring-class m))
           }))))
    
    (catch Throwable t)))"
)

(defvar cider-reflect-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'cider-reflect-follow-link)
    (define-key map [mouse-1] 'cider-reflect-follow-link)
    map))

(defun cider-reflect-follow-link ()
  (interactive)
  (let ((help-echo (get-text-property (point) 'help-echo)))
    (cider-doc-lookup help-echo)))

(defun cider-reflect-typename (tn)
  (propertize
   (let ((parts (split-string tn "\\.")))
     (car (last parts)))
   'font-lock-face 'font-lock-type-face
   'help-echo tn
   'mouse-face 'highlight
   'keymap cider-reflect-link-map
   ))


(defun cider-reflect-on-type (type target-buffer)
  (let* ((class (nrepl-dict-get (cider-var-info type) "class"))
         (response (cider-sync-tooling-eval
                    (format cider-reflect-form class)))
         (value (nrepl-dict-get response "value"))
         (var-info (and value (parseedn-read-str value))))
    (when (hash-table-p var-info)
      (with-current-buffer target-buffer
        
        (save-excursion
          (goto-char (point-max))
          (forward-line -1)
          
          (when-let ((ctors (gethash :constructor var-info)))
            (insert (propertize "Constructors:\n" 'face 'bold))
            (save-restriction
              (narrow-to-region (point) (point))
              (cl-loop
               for c across ctors
               do (insert
                   "("
                   (mapconcat
                    'cider-reflect-typename
                    (gethash :parameters c) " ")
                   ")\n"))
              (align-regexp (point-min) (point-max) "\\( +\\)." nil nil t))
            (insert "\n"))

          (when-let ((methods (gethash :static-method var-info)))
            (insert (propertize "Static methods:\n" 'face 'bold))
            
            (let ((classname (cider-reflect-typename class)))
              (save-restriction
                (narrow-to-region (point) (point))
                (cl-loop
                 for m across methods
                 do
                 (insert
                  "^"
                  (cider-reflect-typename (gethash :returns m))
                  " ("
                  classname "/"
                  (propertize (gethash :name m)
                              'font-lock-face 'font-lock-function-name-face)
                  " "
                  (mapconcat
                   'cider-reflect-typename
                   (gethash :parameters m)
                   " ")
                  ")"
                  "\n"))
                (align-regexp (point-min) (point-max) "\\( +\\)." nil nil t)))
            (insert "\n"))
          
          (when-let ((methods (gethash :method var-info)))
            (insert (propertize "Methods:\n" 'face 'bold))
            
            (save-restriction
              (narrow-to-region (point) (point))
              (cl-loop
               for m across methods
               do
               (insert
                "^"
                (cider-reflect-typename (gethash :returns m))
                " (."
                (propertize (gethash :name m)
                            'font-lock-face 'font-lock-function-name-face)
                " "
                (mapconcat
                 'cider-reflect-typename
                 (gethash :parameters m)
                 " ")
                ")"
                "\n"))
              (align-regexp (point-min) (point-max) "\\( +\\)." nil nil t))
            (insert "\n"))

          (when-let ((fields (gethash :static-field var-info)))
            (insert (propertize "Static Fields:\n" 'face 'bold))
            (let ((classname (cider-reflect-typename class)))
              (save-restriction
                (narrow-to-region (point) (point))
                (cl-loop
                 for f across fields
                 do (insert
                     "^" (cider-reflect-typename (gethash :type f))
                     " " classname "/"
                     (propertize (gethash :name f)
                                 'font-lock-face 'font-lock-variable-name-face)
                     "\n"
                     ))
                (align-regexp (point-min) (point-max) "\\( +\\)." nil nil t)))
            (insert "\n"))
          
          (when-let ((fields (gethash :field var-info)))
            (insert (propertize "Fields:\n" 'face 'bold))
            (save-restriction
              (narrow-to-region (point) (point))
              (cl-loop
               for f across fields
               do (insert
                   "^" (cider-reflect-typename (gethash :type f))
                   " (.-"
                   (propertize (gethash :name f)
                               'font-lock-face 'font-lock-variable-name-face)
                   ")\n"
                   ))
              (align-regexp (point-min) (point-max) "\\( +\\)." nil nil t))
            (insert "\n"))

          
          )))))

(defun add-reflection-to-doc-buffer (o symbol)
  (let ((out-buf (funcall o symbol))
        (inhibit-read-only t)
        (repl (cider-current-repl)))
    (when (and repl (eq (cider-repl-type repl) 'clj))
      (cider-reflect-on-type symbol out-buf))
    out-buf))

(advice-add 'cider-create-doc-buffer
            :around
            'add-reflection-to-doc-buffer)

(provide 'cider-reflect-doc)
