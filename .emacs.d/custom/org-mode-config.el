;; org mode
(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE")))
(setq org-log-done t)
(setq org-agenda-files '("~/Dropbox/memos/agenda"))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((agenda "")
          (alltodo "")))))

;; Customize variables for org journal.
(customize-set-variable 'org-journal-dir "~/Dropbox/memos/journal")
(customize-set-variable 'org-journal-date-format "%A, %d %B %Y")
(require 'org-journal)

;; org babel code executio
;; (setenv "PYTHONPATH" "~/anaconda3/bin")
(setq org-babel-python-command "/home/sameer/anaconda3/bin/python")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (R . t)
   (ruby . t)
   (python . t)
   (shell . t)))
(add-hook 'TeX-mode-hook
          (lambda() (define-key TeX-mode-map "\C-ch" 'helm-bibtex)) )

