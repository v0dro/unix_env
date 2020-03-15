;; Configurations for TRAMP mode.

;; set default of tramp mode to scp
(setq tramp-default-method "scp")
;; turn off TRAMP auto-save
(defun tramp-handle-find-backup-file-name (filename)
  "Like `find-backup-file-name' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-run-real-handler 'find-backup-file-name (list filename))))

(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp "/tmp/"))
