;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exec-path-from-shell
;;;   PATH 等の環境変数を shell の値をもとに設定する
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'exec-path-from-shell)

(custom-set-variables
 '(exec-path-from-shell-variables '("PATH" "MANPATH" "INFOPATH")))

(exec-path-from-shell-initialize)


(provide 'config-exec-path-from-shell)
