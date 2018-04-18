;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display-line-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package display-line-info
  :init
  ;; mode-line に whic-func による関数名表示を行わない
  (require 'which-func)
  (setq orig-which-func-format which-func-format)
  (setq which-func-format "")
  :config
  ;; キーバンド
  :bind
  ("C-c d" . dli-display))


