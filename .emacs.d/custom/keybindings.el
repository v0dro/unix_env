;; Custom keybindings

;; switch between implementation and header.
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; change the binding for C-x o
(global-set-key (kbd "M-o") 'other-window)

;; over-ride default mappings to use helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Org mode keybindings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Set column width to 80.
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))
(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(global-set-key (kbd "C-x ~") 'set-80-columns)

;; set option + ¥ key combo to yield a backslash on a mac
(global-set-key (quote [134217893]) "\\")


