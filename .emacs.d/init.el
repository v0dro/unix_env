;; my emacs config.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; ----------------------------------------------------------------
;; install packages without user intervention
(setq package-list '(better-defaults markdown-mode smartparens enh-ruby-mode inf-ruby helm projectile helm-projectile auto-complete markdown-toc helm-ag))
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
(add-to-list
 'custom-theme-load-path "~/.emacs.d/themes/dracula" )
(load-theme 'dracula t)

;; inhibit start screen
(setq inhibit-startup-screen t)

;; require packages
(require 'better-defaults)
(require 'markdown-mode)
(require 'smartparens-config)

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; initialize modes
(smartparens-global-mode t)             ;smart parens
(global-linum-mode t)                   ;line numbers on

;; over-ride default mappings to use helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

;; use space instead of tabs
(setq tab-width 2) 
(setq-default indent-tabs-mode nil)

;; change the binding for C-x o
(global-set-key (kbd "M-o") 'other-window)

;; set column mode to true
(setq column-number-mode t)

;; global auto-revert-mode
(global-auto-revert-mode 1)

;; enable auto-complete mode in all buffers except minibuffer
(defun auto-complete-mode-maybe ()
  "No maybe for you. Only AC!"
  (unless (minibufferp (current-buffer))
    (auto-complete-mode 1)))

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

;; which function mode
(which-function-mode 1)
