(let ((backup-dir (expand-file-name "backup" user-emacs-directory)))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir)
    (set-file-modes backup-dir 448))

  (setq make-backup-files t
        delete-old-versions t
        kept-old-versions 2
        kept-new-versions 8
        version-control t
        backup-directory-alist `(("." . ,backup-dir))))

