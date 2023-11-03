;; my emacs config.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
	("elpa" . "http://tromey.com/elpa/")))

;;(package-refresh-contents)
;; ----------------------------------------------------------------
;; install packages without user intervention
(setq package-list '(better-defaults
                     dracula-theme
                     markdown-mode
                     smartparens
                     enh-ruby-mode
                     inf-ruby
                     helm
                     helm-gtags
                     projectile
                     helm-projectile
                     markdown-toc
                     use-package
                     yasnippet
                     page-break-lines
                     dashboard
                     yard-mode
                     ansi-color
                     org-journal
                     rvm
                     ))
; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
;; ----------------------------------------------------------------

;; load all .el files in the custom/ directory
;; (load-file "~/.emacs.d/custom/utils.el")
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/custom/")

;; initialize with dracula theme
(load-theme 'dracula t)

;; require use-package
(eval-when-compile
  (require 'use-package))

(use-package better-defaults)
(use-package markdown-mode)
(use-package smartparens-config)

(helm-mode 1)

(require 'helm-projectile)
(require 'setup-helm-gtags)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)

;; initialize modes
(smartparens-global-mode t)             ;smart parens
(global-display-line-numbers-mode t)                   ;line numbers on

;; use space instead of tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

;; set column mode to true
(setq column-number-mode t)

;; global auto-revert-mode
(global-auto-revert-mode 1)

(put 'erase-buffer 'disabled nil)

;; which function mode in top header line
(which-function-mode)

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))
(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (yas-reload-all))

;; ;; use column-marker
(require 'column-marker)
(add-hook 'after-init-hook (lambda () (interactive) (column-marker-1 80)))

;; projectile basic config
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; emacs dashboard setup
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 10)
                        (projects . 10)
                        (agenda . 5)))

(setq load-path (cons (expand-file-name "~/.emacs.d/cmake-mode-20231006.1308") load-path))
(require 'cmake-mode)

;; ansi color for compilation buffer
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;; delete trailing whitespace on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; highlight indents
(add-to-list 'load-path "~/.emacs.d/highlight-indents/")
(require 'highlight-indentation)

(setq make-backup-files nil)

(add-to-list 'auto-mode-alist '("\\.jdf\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ci\\'" . c++-mode))

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:abci:")
                   "remote-shell" "/bin/bash"))

(setq x-select-enable-primary nil)
