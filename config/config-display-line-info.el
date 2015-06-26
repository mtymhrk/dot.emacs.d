;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display-line-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'display-line-info)

;;; mode-line に whic-func による関数名表示を行わない
(require 'which-func)
(setq orig-which-func-format which-func-format)
(setq which-func-format "")

;;; キーバンド
(global-set-key (kbd "C-c d") 'dli-display)



(provide 'config-display-line-info)
