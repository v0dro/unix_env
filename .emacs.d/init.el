;; my emacs config.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

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
                     company
                     markdown-toc
                     use-package
                     yasnippet
                     page-break-lines
                     dashboard
                     yard-mode
                     cuda-mode
                     ansi-color
                     org-journal
                     pdf-tools
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

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(global-set-key (kbd "C-x ~") 'set-80-columns)

;; load all .el files in the custom/ directory
;; (load-file "~/.emacs.d/custom/utils.el")
(load-file "~/.emacs.d/custom/mozc.el")
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

(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)
(require 'setup-helm-gtags)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

;; enable reftex with auctex
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(setq reftex-plug-into-AUCTeX t) ; Anleitung S.
(setq reftex-default-bibliography '(
                                    "/home/sameer/gitrepos/sameer-hpc-asia-2020/sameer_ref.bib"
                                    ))
(add-hook 'TeX-mode-hook
          (lambda() (define-key TeX-mode-map "\C-ch" 'helm-bibtex)) )

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
(setq js-indent-level 2)

;; change the binding for C-x o
(global-set-key (kbd "M-o") 'other-window)

;; set column mode to true
(setq column-number-mode t)

;; global auto-revert-mode
(global-auto-revert-mode 1)

;; company mode for auto complete
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete-common)

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

(eval-after-load 'enh-ruby-mode
  '(progn
     (define-key enh-ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))

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
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))
(setq org-log-done t)
(setq org-agenda-files (list
                        "~/Dropbox/memos/2019/october/actions_list.org"
                        "~/Dropbox/memos/2019/october/notes.org"
                        ))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

;; scala mode enable
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; set default of tramp mode to ssh
(setq tramp-default-method "ssh")

;; doc-view mode default continuos scrolling
(setq doc-view-continuous 1)

;; set option + ¥ key combo to yield a backslash on a mac
(global-set-key (quote [134217893]) "\\")

;; ansi color for compilation buffer
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;; switch between implementation and header.
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config (setq neo-autorefresh nil))

;; latex 
(require 'ox-latex)

(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;; pdf-tools configuration
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t
      )
;; revert pdf-view after compilation
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(use-package pdf-tools
 :pin manual ;; manually update
 :config
 ;; initialise
 (pdf-tools-install)
 ;; open pdfs scaled to fit page
 (setq-default pdf-view-display-size 'fit-page)
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t)
 ;; use normal isearch
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;; robe mode
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(put 'erase-buffer 'disabled nil)

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)

(add-to-list 'auto-mode-alist '("\\.jdf\\'" . c-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-dir "~/Dropbox/memos/journal")
 '(package-selected-packages
   (quote
    (cmake-mode yasnippet-snippets yard-mode yaml-mode xcscope web-mode vue-mode use-package sx smartparens scala-mode rtags robe pomidor pdf-tools org-journal neotree multi-term moz-controller markdown-toc magit helm-projectile helm-gtags helm-bibtex enh-ruby-mode emmet-mode elpy dracula-theme dashboard cuda-mode better-defaults auto-complete auctex aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
