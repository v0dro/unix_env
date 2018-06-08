;; my emacs config.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-refresh-contents)
;; ----------------------------------------------------------------
;; install packages without user intervention
(setq package-list '(better-defaults
                     dracula-theme
                     markdown-mode
                     smartparens
                     enh-ruby-mode
                     inf-ruby
                     helm
                     projectile
                     helm-projectile
                     company
                     markdown-toc
                     use-package
                     yasnippet
                     page-break-lines
                     dashboard
                     yard-mode
                     scala-mode
                     cuda-mode))
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

;; initialize with dracula theme
(load-theme 'dracula t)

;; require use-package
(eval-when-compile
  (require 'use-package))

(use-package better-defaults)
(use-package markdown-mode)
(use-package smartparens-config)

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; load all .el files in the custom/ directory
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/custom/")

;; initialize modes
(smartparens-global-mode t)             ;smart parens
(global-linum-mode t)                   ;line numbers on

;; over-ride default mappings to use helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; use space instead of tabs
(setq tab-width 2) 
(setq-default indent-tabs-mode nil)

;; change the binding for C-x o
(global-set-key (kbd "M-o") 'other-window)

;; set column mode to true
(setq column-number-mode t)

;; global auto-revert-mode
(global-auto-revert-mode 1)

;; company mode for auto complete
(add-hook 'after-init-hook 'global-company-mode)

;; In Ruby mode, patch the # key to make {} when inside a string
(defun senny-ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))

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

;; yasnippet configs
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (yas-reload-all))

;; ;; use column-marker
(require 'column-marker)
(add-hook 'after-init-hook (lambda () (interactive) (column-marker-1 80)))
 
;; emacs dashboard setup
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items '((recents  . 10)
                        (projects . 10)
                        (agenda . 5)))

;; YARD mode config
(require 'yard-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'eldoc-mode)

;; rex mode for oedipus_lex files ending in .rex
(require 'rex-mode)

;; org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; scala mode enable
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; set default of tramp mode to ssh
(setq tramp-default-method "ssh")

;; doc-view mode default continuos scrolling
(setq doc-view-continuous 1)

;; hippie expand completions list
(setq hippie-expand-try-functions-list '(
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; set option + ¥ key combo to yield a backslash on a mac
(global-set-key (quote [134217893]) "\\")

;; set org mode agenda
(setq org-agenda-files (list "~/Dropbox/memos/2018/june/todos.org"))
