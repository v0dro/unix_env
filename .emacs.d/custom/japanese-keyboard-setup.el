;; ;; setup mozc japanese keyboard (be sure to install emacs-mozc-bin pacakge on ubuntu)
(if (not (require 'mozc nil t))
    (message "cannot find mozc keyboard")
  (setq default-input-method "japanese-mozc")
  (setq mozc-candidate-style 'overlay))

