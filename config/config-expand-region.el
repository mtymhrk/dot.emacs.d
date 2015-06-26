;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region.el
;;;   https://github.com/magnars/expand-region.el.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; git clone https://github.com/magnars/expand-region.el.git

(require 'expand-region)
;; (global-set-key (kbd "M-M") 'er/expand-region)
;; (global-set-key (kbd "C-M-M") 'er/contract-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
;; (global-set-key (kbd "C-M-H-SPC") 'er/contract-region)

(custom-set-variables '(expand-region-contract-fast-key "M-SPC"))

(provide 'config-expand-region)
