;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expand-region.el
;;;   https://github.com/magnars/expand-region.el.git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; git clone https://github.com/magnars/expand-region.el.git

(require 'expand-region)

(defvar my-orig-C-M-SPC-command (global-key-binding (kbd "C-M-SPC")))
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(custom-set-variables '(expand-region-contract-fast-key "M-SPC"))

;; (eval-after-load 'config-extend-kill-region
;;   '(progn
;;      (setq kill-ring-save-or-do-something (lambda () (er/expand-region 1)))
;;      (custom-set-variables '(expand-region-contract-fast-key "W"))
;;      (global-set-key (kbd "C-M-SPC") my-orig-C-M-SPC-command)))

(provide 'config-expand-region)
