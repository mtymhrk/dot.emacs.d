;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WoMan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'woman)

(setq woman-use-own-frame nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin for Woman-mode

(eval-after-load 'config-popwin
  '(progn
     ;;; WoMan バッファをポップアップで表示
     (add-popwin-special-display-config '("^\\*WoMan" :regexp t :noselect t :stick t))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popwin for Man-mode

(eval-after-load 'config-popwin
  '(progn
     ;;; Man バッファをポップアップで表示
     (add-popwin-special-display-config '(Man-mode :noselect t :stick t))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-woman)
