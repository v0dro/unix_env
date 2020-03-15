(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (message (car list))
    (setq list (cdr list))))

;; (defun list-dirs-recursive (dir)
;;   "recursively visit all the folders within a folder."
;;   (remove-if-not 'file-directory-p (directory-files dir))
;;   ;; get all files and folders within dir
;;   ;; take only the folders 
;;   )

;; (message (length (list-dirs-recursive "/home/sameer/Dropbox/memos/research")))
;; ;; (insert (list-dirs-recursive "/home/sameer/Dropbox/memos/"))
;; ;; (print-elements-of-list (directory-files  "/home/sameer/Dropbox/memos")
