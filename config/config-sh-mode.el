;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sh-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'use-package))

(use-package sh-script
  :commands sh-mode
  :config
  (defun my-hook-sh-mode--0 ()
    (setq sh-basic-offset 2) ; インデント幅の設定
    (setq sh-indentation 2)) ; インデント幅の設定

  (add-hook 'sh-mode-hook #'my-hook-sh-mode--0)

  (use-package fill-column-indicator
    :init
    (defun my-hook-sh-mode--fci ()
      (setq fill-column 80)
      (fci-mode 1))
    :hook
    ((sh-mode . my-hook-sh-mode--fci))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-sh-mode)
