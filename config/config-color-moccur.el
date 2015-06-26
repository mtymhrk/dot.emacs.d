;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; color-moccur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-moccur)
(setq moccur-split-word t)
(setq dmoccur-exclusion-mask
      (append '("\\.o$" "\\.so$" "\\.hg/.+" "GPATH" "GRTAGS" "GSYMS" "GTAGS")
              dmoccur-exclusion-mask))
(eval-after-load "color-moccur"
  '(require 'moccur-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin for color-moccur

(eval-after-load 'config-popwin
  '(progn
     ;;; color-moccur の検索結果をポップアップで表示
     (add-popwin-special-display-config '(moccur-mode :noselect t :stick t))
     (add-popwin-special-display-config '(moccur-grep-mode :noselect t :stick t))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-color-moccur)
