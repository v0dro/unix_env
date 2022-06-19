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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Save the running clock and all clock history when exiting Emacs, load it on  startup
(setq org-clock-persist t)
;; report time only in hours
(setq org-duration-format (quote h:mm))
