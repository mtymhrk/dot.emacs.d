;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region.el
;;;   https://github.com/magnars/expand-region.el.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; git clone https://github.com/magnars/expand-region.el.git

(require 'expand-region)

(define-key keymap-ctrl-meta-space (kbd "r") 'er/expand-region)
(custom-set-variables '(expand-region-contract-fast-key "R"))

(provide 'config-expand-region)
