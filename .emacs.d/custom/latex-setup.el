;; setup auctex and latex commands and defaults
(if (not (require `tex-site nil t))
    (message "tex-site not found")
  (require `tex-style)
  (add-hook `LaTeX-mode-hook `turn-on-reftex)
  
  ;; spellcheck in LaTex mode
  (add-hook `latex-mode-hook `flyspell-mode)
  (add-hook `tex-mode-hook `flyspell-mode)
  (add-hook `bibtex-mode-hook `flyspell-mode)
  
  ;; Math mode for LaTex
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  
  ;; make sure auctex macros are loaded automatically
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  
  ;; default indent level
  (setq LaTeX-indent-level 2))
