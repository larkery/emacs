(defun face-rgb-color (face attr)
  (let* ((colr (face-attribute face attr)))
    (unless (eq colr 'unspecified)
      (let* ((rgb (color-name-to-rgb colr))
             (hsl (apply 'color-rgb-to-hsl rgb))
             (hsl2 (list (+ 0.05 (nth 0 hsl)) (min 1.0 (+ 0 (nth 1 hsl))) (min 1.0 (+ 0 (nth 2 hsl)))))
             (rgb2 (apply 'color-hsl-to-rgb hsl2))
             (result (apply 'color-rgb-to-hex rgb2)))
        result
        ))))

(defun emacs-color-to-hex (color)
  (let ((float-rgb (color-name-to-rgb color)))
    (color-rgb-to-hex
     (nth 0 float-rgb)
     (nth 1 float-rgb)
     (nth 2 float-rgb) 2)))

(defun theme-to-xresources (&rest _blah)
  "Generate and update xresources from current theme"
  (interactive)

  (when (display-graphic-p)
    (require 'term)
    (with-current-buffer (get-buffer-create "*gen-resources*")
      (erase-buffer)
      (cl-loop
       for term in '("XTerm" "URxvt")
       do (cl-loop
           for spec in '(("background" default :background)
                         ("borderColor" default :background)
                         ("foreground" default :foreground)
                         ("cursorColor" cursor :background)
                         ;; normal versions of colors
                         ("color0" term-color-black :background)
                         ("color1" term-color-red :background)
                         ("color2" term-color-green :background)
                         ("color3" term-color-yellow :background)
                         ("color4" term-color-blue :background)
                         ("color5" term-color-magenta :background)
                         ("color6" term-color-cyan :background)
                         ("color7" term-color-white :background))
           do (cl-destructuring-bind
                  (resource face attr) spec
                (let ((att (face-attribute face attr)))
                  (unless (eq att 'unspecified)
                    (insert (format "%s*%s: %s\n" term resource att))))))

       do (cl-loop
           for spec in '(("color8" term-color-black :background)
                         ("color9" term-color-red :background)
                         ("color10" term-color-green :background)
                         ("color11" term-color-yellow :background)
                         ("color12" term-color-blue :background)
                         ("color13" term-color-magenta :background)
                         ("color14" term-color-cyan :background)
                         ("color15" term-color-white :background))
           do (cl-destructuring-bind
                  (resource face attr) spec
                (let ((nam (face-rgb-color face attr)))
                  (when nam
                    (insert (format "%s*%s: %s\n" term resource nam)))))))

      ;; (insert (format "*Foreground: %s\n*Background: %s\n"
      ;;                 (face-attribute 'default :foreground)
      ;;                 (face-attribute 'default :background)))

      (if (eq 'light (frame-parameter nil 'background-mode))
          (insert "#include \".Xresources_i3_light\"\n")
        (insert "#include \".Xresources_i3_dark\"\n"))

      (let ((weight (face-attribute 'default :weight)))
        (when weight
          (insert (format "URxvt.font: xft:Monospace:size=12:weight=%s\n" weight))
          (insert (format "URxvt.boldFont: xft:Monospace:size=12:weight=bold"))))
      (insert "\n")
      
      (goto-char (point-min))
      (unless (search-forward "unspecified" nil t)
        (let ((default-directory (getenv "HOME")))
          (call-process-region
           (point-min)
           (point-max)
           "xrdb"
           nil nil nil
           "-merge"))
        (write-region (point-min) (point-max) "~/.Xresources_emacs")
        (remove-hook 'window-configuration-change-hook 'theme->xresources))
      (kill-buffer)))
  t)

(provide 'theme-to-xresources)
