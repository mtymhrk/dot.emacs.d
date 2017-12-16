;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sh-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sh-script)

(defun sh-mode-hook--0 ()
  (setq sh-basic-offset 2) ; インデント幅の設定
  (setq sh-indentation 2)) ; インデント幅の設定

(add-hook 'sh-mode-hook 'sh-mode-hook--0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1 行の文字数上限可視化する

(with-eval-after-load 'fill-column-indicator
  (defun sh-mode-hook--fci ()
    (setq fill-column 80)
    (fci-mode 1))
  (add-hook 'sh-mode-hook 'sh-mode-hook--fci))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'config-sh-mode)
