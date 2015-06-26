;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SmartParens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smartparens)
(require 'smartparens-config)

(custom-set-variables
 ;; カッコが自動挿入された際に、ハイライトを行わない
 '(sp-highlight-pair-overlay nil))

(smartparens-global-mode)

(provide 'config-smartparens)
