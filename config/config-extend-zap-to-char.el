;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extend-zap-to-char.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'extend-zap-to-char)

(eval-after-load 'config-ace-jump-mode
  '(progn
     (setq eztc:printing-char-commond 'ace-jump-char-mode)))

(global-set-key (kbd "M-z") 'extend-zap-to-char)


