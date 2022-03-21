;; setup auctex and latex commands and defaults

;; enable reftex with auctex
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(setq reftex-plug-into-AUCTeX t) ; Anleitung S.
(setq reftex-default-bibliography '(
                                    "/home/sameer/bib-file.bib"
                                    "/Users/sameer/rioyokotalab.bib"
                                    ))

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

(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
