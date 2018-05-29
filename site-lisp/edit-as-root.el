(defun edit-as-root (prefix)
  (interactive "P")

  (let ((target-user (if prefix
                         (completing-read "edit as: " '("root" "nixops"))
                         "root"))
        (the-place (or buffer-file-name default-directory))
        (position (point)))
    (if (file-remote-p the-place)
        (let* ((dat (tramp-dissect-file-name the-place))
               (u (tramp-file-name-user dat))
               (m1 (tramp-file-name-method dat))
               (m (if (string= m1 "scp") "ssh" m1))
               (h (tramp-file-name-host dat))
               (l (tramp-file-name-localname dat))

               (sudo-path (concat
                           tramp-prefix-format

                           (unless (zerop (length m))
                             (concat m tramp-postfix-method-format))

                           (unless (zerop (length u))
                             (concat u tramp-postfix-user-format))

                           (when h
                             (if (string-match tramp-ipv6-regexp h)
                                 (concat tramp-prefix-ipv6-format h tramp-postfix-ipv6-format)
                               h))

                           tramp-postfix-hop-format

                           "sudo:" target-user "@" h

                           tramp-postfix-host-format

                           l)))

          (find-alternate-file sudo-path))
      ;; non-remote files are easier
      (find-alternate-file (concat "/sudo:root@" (system-name) ":" the-place)))
    (goto-char position)))

(provide 'edit-as-root)
